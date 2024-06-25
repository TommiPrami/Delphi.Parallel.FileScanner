unit TestAdaptersFireDAC;

interface

uses
  Classes,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Error,
  Spring,
  Spring.Mocking,
  Spring.Persistence.Adapters.FireDAC,
  Spring.Persistence.Core.Base,
  Spring.Persistence.Core.Exceptions,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.Session,
  Spring.Persistence.Core.ResourceStrings,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Generators.Ansi,
  Spring.Persistence.SQL.Params,
  Spring.TestUtils,
  SysUtils,
  TestEntities,
  TestFireDACConnection,
  TestFramework
  ;

type
  TFireDACExceptionHandlerAccess = class(TFireDACExceptionHandler);
  TFireDACExceptionHandlerTest = class(TTestCase<TFireDACExceptionHandlerAccess>)
  private
    function CreateException(kind: TFDCommandExceptionKind): EFDDBEngineException;
  published
    procedure TestGetAdapterException_EFDDBEngineException_ekUKViolated_Returns_EORMConstraintException;
    procedure TestGetAdapterException_EFDDBEngineException_ekFKViolated_Returns_EORMConstraintException;
    procedure TestGetAdapterException_EFDDBEngineException_Others;
    procedure TestGetAdapterException_EDatabaseError;
    procedure TestGetAdapterException_Others_Return_Nil;
  end;

  TBaseFireDACConnectionAdapterTest = class(TTestCase)
  strict protected
    fConnection: IDBConnection;
    fMockConnection: Mock<TTestFDConnection>;
    fSqliteConnection: TFDConnection;
    procedure CreateSqliteConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFireDACConnectionAdapterTest = class(TBaseFireDACConnectionAdapterTest)
  protected
    procedure SetUp; override;
  published
    procedure TestIsConnected;
    procedure TestConnect;
    procedure TestConnectException;
    procedure TestDisconnect;
    procedure TestCreateStatement;
    procedure TestBeginTransaction;
  end;

  TFireDACTransactionAdapterTest = class(TBaseFireDACConnectionAdapterTest)
  private
    fTransaction: IDBTransaction;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCommit;
    procedure TestCommit_Exception;
    procedure TestRollback;
    procedure TestRollback_Exception;
  end;

  TFireDACStatementAdapterTest = class(TBaseFireDACConnectionAdapterTest)
  private
    fStatement: IDBStatement;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExecute_Exception;
    procedure TestExecuteQuery_Exception;
  end;

  [Table('CUSTOMERS')]
  TFDCustomer = class
  private
    FId: Integer;
    FAge: Integer;
    FName: string;
    FHeight: Nullable<Double>;
  public
    [Column('ID', [cpPrimaryKey])] [AutoGenerated] property Id: Integer read FId write FId;
    [Column] property Age: Integer read FAge write FAge;
    [Column] property Name: string read FName write FName;
    [Column] property Height: Nullable<Double> read FHeight write FHeight;
  end;

  TFireDACSessionTest = class(TTestCase)
  private
    FConnection: IDBConnection;
    FSession: TSession;
    FDACConnection: TFDConnection;
  protected
    procedure CreateTables;
    function CreateCustomer(const name: string; const age: Integer): TFDCustomer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Save;
    procedure SaveNullable;
    procedure WhenSavingInTransaction_RollbackIsSuccessful;
    procedure WhenSavingInTransaction_CommitIsSuccessful;
    procedure NestedTransaction;
  end;

implementation

{$I Spring.inc}

uses
  Spring.Persistence.Core.ConnectionFactory,
  FireDAC.Phys.SQLite,
  {$IFDEF DELPHIX_SYDNEY_UP}
  FireDAC.Phys.SQLiteWrapper.Stat,
  {$ENDIF}
  Spring.Persistence.SQL.Interfaces,
  Spring.Collections,
  TestSession,
  Variants;


{$REGION 'TFireDACExceptionHandlerTest'}

function TFireDACExceptionHandlerTest.CreateException(
  kind: TFDCommandExceptionKind): EFDDBEngineException;
begin
  Result := EFDDBEngineException.Create;
  Result.AppendError(0, Ord(kind), '', '', kind, 0, 0);
end;

procedure TFireDACExceptionHandlerTest.TestGetAdapterException_EDatabaseError;
var
  exc, result: Shared<Exception>;
begin
  exc := EDatabaseError.Create('');
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, EFireDACAdapterException);
  CheckEqualsString('message', result.Value.Message);
  CheckFalse(EFireDACAdapterException(result.Value).ErrorCode.HasValue);
end;

procedure TFireDACExceptionHandlerTest.TestGetAdapterException_EFDDBEngineException_ekFKViolated_Returns_EORMConstraintException;
var
  exc, result: Shared<Exception>;
begin
  exc := CreateException(ekFKViolated);
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, EORMConstraintException);
  CheckEqualsString('message', result.Value.Message);
  CheckEquals(Ord(ekFKViolated), EORMConstraintException(result.Value).ErrorCode);
end;

procedure TFireDACExceptionHandlerTest.TestGetAdapterException_EFDDBEngineException_ekUKViolated_Returns_EORMConstraintException;
var
  exc, result: Shared<Exception>;
begin
  exc := CreateException(ekUKViolated);
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, EORMConstraintException);
  CheckEqualsString('message', result.Value.Message);
  CheckEquals(Ord(ekUKViolated), EORMConstraintException(result.Value).ErrorCode);
end;

procedure TFireDACExceptionHandlerTest.TestGetAdapterException_EFDDBEngineException_Others;
var
  exc, result: Shared<Exception>;
begin
  exc := CreateException(ekInvalidParams);
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, EFireDACAdapterException);
  CheckEqualsString('message', result.Value.Message);
  CheckEquals(Ord(ekInvalidParams), EFireDACAdapterException(result.Value).ErrorCode);
end;

procedure TFireDACExceptionHandlerTest.TestGetAdapterException_Others_Return_Nil;
var
  exc, result: Shared<Exception>;
begin
  exc := Exception.Create('');
  result := SUT.GetAdapterException(exc, '');
  CheckNull(result);
end;

{$ENDREGION}


{$REGION 'TBaseFireDACConnectionAdapterTest'}

procedure TBaseFireDACConnectionAdapterTest.CreateSqliteConnection;
begin
  fSqliteConnection := TFDConnection.Create(nil);
  fSqliteConnection.DriverName := 'SQLite';
  fConnection := TFireDACConnectionAdapter.Create(fSqliteConnection);
  fConnection.AutoFreeConnection := True;
end;

procedure TBaseFireDACConnectionAdapterTest.SetUp;
begin
  inherited;
  fMockConnection := Mock<TTestFDConnection>.Create(TMockBehavior.Strict);
  fConnection := TFireDACConnectionAdapter.Create(fMockConnection);
  fConnection.AutoFreeConnection := False;
end;

procedure TBaseFireDACConnectionAdapterTest.TearDown;
begin
  fConnection := nil;
  fMockConnection.Behavior := TMockBehavior.Dynamic;
  fMockConnection.Free;
  fSqliteConnection := nil;
  inherited;
end;

{$ENDREGION}


{$REGION 'TFireDACConnectionAdapterTest'}

procedure TFireDACConnectionAdapterTest.SetUp;
begin
  inherited;

end;

procedure TFireDACConnectionAdapterTest.TestBeginTransaction;
var
  transaction: IDBTransaction;
begin
  // Test connect exception
  fMockConnection.Setup.Raises<EFDException>.When.SetConnected(True);
  CheckException(EFireDACAdapterException,
    procedure begin fConnection.BeginTransaction end);

  CreateSqliteConnection;
  transaction := fConnection.BeginTransaction;
  CheckNotNull(transaction);

  // Test InTransaction no nesting exception
  fSqliteConnection.TxOptions.EnableNested := False;
  CheckException(EFireDACAdapterException,
    procedure begin fConnection.BeginTransaction end);
end;

procedure TFireDACConnectionAdapterTest.TestConnect;
begin
  fMockConnection.Setup.Executes.When.SetConnected(True);
  fConnection.Connect;
  fMockConnection.Received(1).SetConnected(True);

  Pass;
end;

procedure TFireDACConnectionAdapterTest.TestConnectException;
var
  adapter: TFireDACConnectionAdapter;
begin
  // Use native connection for this test
  adapter := TFireDACConnectionAdapter.Create(TFDConnection.Create(nil));
  try
    adapter.AutoFreeConnection := True;
    ExpectedException := EFireDACAdapterException;
    adapter.Connect;
  finally
    adapter.Free;
  end;
end;

procedure TFireDACConnectionAdapterTest.TestCreateStatement;
var
  returnValue: IDBStatement;
begin
  CreateSqliteConnection;

  returnValue := fConnection.CreateStatement;

  CheckNotNull(returnValue);
end;

procedure TFireDACConnectionAdapterTest.TestDisconnect;
begin
  fMockConnection.Setup.Raises<EFDException>.When.SetConnected(False);
  CheckException(EFireDACAdapterException,
    procedure begin fConnection.Disconnect end);

  fMockConnection.Setup.Executes.When.SetConnected(False);
  fConnection.Disconnect;
  fMockConnection.Received(2).SetConnected(False);

  Pass;
end;

procedure TFireDACConnectionAdapterTest.TestIsConnected;
begin
  fMockConnection.Setup.Returns<Boolean>(False).When.GetConnected;
  CheckFalse(fConnection.IsConnected);

  fMockConnection.Setup.Returns<Boolean>(True).When.GetConnected;
  CheckTrue(fConnection.IsConnected);
end;

{$ENDREGION}


{$REGION 'TFireDACTransactionAdapterTest'}

procedure TFireDACTransactionAdapterTest.SetUp;
begin
  inherited;
  CreateSqliteConnection;
  fTransaction := fConnection.BeginTransaction;
end;

procedure TFireDACTransactionAdapterTest.TearDown;
begin
  fTransaction := nil;
  inherited;
end;

procedure TFireDACTransactionAdapterTest.TestCommit;
begin
  fTransaction.Commit;
  Pass;
end;

procedure TFireDACTransactionAdapterTest.TestCommit_Exception;
begin
  fSqliteConnection.Connected := False;
//  ExpectedException := EFireDACAdapterException;
  fTransaction.Commit;
  Pass;
end;

type
  TMockedFireDACTransactionAdapter = class(TFireDACTransactionAdapter)
  private
    fInTransaction: Boolean;
  public
    function InTransaction: Boolean; override;
  end;

function TMockedFireDACTransactionAdapter.InTransaction: Boolean;
begin
  // We need to return true to let the Rollback fail since we closed the
  // connection.
  Result := fInTransaction;
end;

procedure TFireDACTransactionAdapterTest.TestRollback;
begin
  fTransaction.Rollback;
  Pass;
end;

procedure TFireDACTransactionAdapterTest.TestRollback_Exception;
begin
  fSqliteConnection.Connected := False;
//  ExpectedException := EFireDACAdapterException;
  fTransaction.Rollback;
  Pass;
end;

{$ENDREGION}


{$REGION 'TFireDACStatementAdapterTest'}

procedure TFireDACStatementAdapterTest.SetUp;
begin
  inherited;
  CreateSqliteConnection;
  fStatement := fConnection.CreateStatement;
end;

procedure TFireDACStatementAdapterTest.TearDown;
begin
  fStatement := nil;
  inherited;
end;

procedure TFireDACStatementAdapterTest.TestExecuteQuery_Exception;
var
  resultSet: IDBResultSet;
begin
  fSqliteConnection.Connected := False;
  try
    resultSet := fStatement.ExecuteQuery;
    Fail('Exception expected');
  except
    on E: Exception do
    begin
      CheckIs(E, EFireDACAdapterException);
      CheckNotNull(E.InnerException);
      CheckEqualsString(Format(SCannotOpenQuery,
        [E.InnerException.Message]), E.Message);
      resultSet := nil;
    end;
  end;
end;

procedure TFireDACStatementAdapterTest.TestExecute_Exception;
begin
  fSqliteConnection.Connected := False;
  ExpectedException := EFireDACAdapterException;
  fStatement.Execute;
end;

{$ENDREGION}


{$REGION 'TFireDACSessionTest'}

function TFireDACSessionTest.CreateCustomer(const name: string;
  const age: Integer): TFDCustomer;
begin
  Result := TFDCustomer.Create;
  Result.Name := name;
  Result.Age := age;
end;

procedure TFireDACSessionTest.CreateTables;
begin
  FDACConnection.ExecSQL('CREATE TABLE IF NOT EXISTS CUSTOMERS ([ID] INTEGER PRIMARY KEY, [AGE] INTEGER NULL,'+
    '[NAME] NVARCHAR (255), [HEIGHT] FLOAT, [PICTURE] BLOB); ');
end;

procedure TFireDACSessionTest.NestedTransaction;
const
  __SELECT_COUNT_QUERY = 'select * from CUSTOMERS';
var
  LSession: TSession;
  LDACConnection: TFDConnection;
  LConnection: IDBConnection;

  LEntity: TFDCustomer;
  LRecordCount: Integer;
  LDBTransaction: IDBTransaction;
  LDBTransactionNested: IDBTransaction;
begin
  LDACConnection := TFDConnection.Create(nil);
  LDACConnection.DriverName := 'SQLite';
  LDACConnection.Params.Add('Database=file::memory:?cache=shared');
  LConnection := TConnectionFactory.GetInstance(dtFireDAC, LDACConnection);
  LConnection.AutoFreeConnection := True;
  LConnection.QueryLanguage := qlSQLite;
  LSession := TSession.Create(LConnection);
  try
    LRecordCount := LSession.ExecuteScalar<Integer>(__SELECT_COUNT_QUERY, []);
    CheckEquals(0, LRecordCount);
    LDBTransaction := FSession.BeginTransaction;
    try
      LDBTransaction.TransactionName := 'TestTransactionName';
      LDBTransactionNested := FSession.BeginTransaction;
      try
        LDBTransactionNested.TransactionName := 'TestNestedTransactionName';
        LEntity := TFDCustomer.Create;
        try
          LEntity.Name := 'TestTransactionComit';
          FSession.Insert(LEntity);
//          LRecordCount := LSession.ExecuteScalar<Integer>(__SELECT_COUNT_QUERY, []);
          CheckEquals(0, LRecordCount);
          LDBTransactionNested.Commit;
//          LRecordCount := LSession.ExecuteScalar<Integer>(__SELECT_COUNT_QUERY, []);
          CheckEquals(0, LRecordCount);
        finally
          LEntity.Free;
        end;
      finally
        LDBTransactionNested := nil;
      end;
      LDBTransaction.Commit;
      LRecordCount := LSession.ExecuteScalar<Integer>(__SELECT_COUNT_QUERY, []);
      CheckEquals(1, LRecordCount);
    finally
      LDBTransaction := nil;
    end;
  finally
    LSession.Free;
  end;
end;

procedure TFireDACSessionTest.Save;
var
  customer: TFDCustomer;
begin
  customer := CreateCustomer('Foo', 25);

  FSession.Save(customer);

  CheckEquals('Foo', FSession.FindAll<TFDCustomer>.First.Name);
  CheckFalse(FSession.FindAll<TFDCustomer>.First.Height.HasValue);
  customer.Free;
end;

procedure TFireDACSessionTest.SaveNullable;
const
  OnePointOne: Double = 1.1;
var
  customer: TFDCustomer;
begin
  customer := CreateCustomer('Foo', 25);
  customer.Height := OnePointOne;
  FSession.Save(customer);

  CheckEquals(OnePointOne, FSession.FindAll<TFDCustomer>.First.Height);
  customer.Free;
end;

procedure TFireDACSessionTest.SetUp;
begin
  FDACConnection := TFDConnection.Create(nil);
  FDACConnection.DriverName := 'SQLite';
  FDACConnection.Params.Add('Database=file::memory:?cache=shared');
//  FDACConnection.Params.Add('Database=:memory:');
//  inherited SetUp;
  FConnection := TConnectionFactory.GetInstance(dtFireDAC, FDACConnection);
  FConnection.AutoFreeConnection := True;
  FConnection.QueryLanguage := qlSQLite;
  FConnection.AddExecutionListener(
    procedure(const command: string; const params: IEnumerable<TDBParam>)
    var
      i: Integer;
      param: TDBParam;
    begin
      Status(command);
      i := 0;
      for param in params do
      begin
        Status(Format('%2:d %0:s = %1:s. Type: %3:s',
          [param.Name,
          PrettyPrintVariant(param.ToVariant),
          i,
          VarTypeAsText(VarType(param.ToVariant))]));
        Inc(i);
      end;
      Status('-----');
    end);
  FSession := TSession.Create(FConnection);
  CreateTables;
end;

procedure TFireDACSessionTest.TearDown;
begin
  inherited TearDown;
  FSession.Free;
  FDACConnection := nil;
  FConnection := nil;
end;

procedure TFireDACSessionTest.WhenSavingInTransaction_CommitIsSuccessful;
var
  customer: TFDCustomer;
  transaction: IDBTransaction;
begin
  customer := CreateCustomer('Foo', 25);

  transaction := FSession.BeginTransaction;
  FSession.Save(customer);
  customer.Free;

  CheckEquals('Foo', FSession.FindAll<TFDCustomer>.First.Name);

  transaction.Commit;
  CheckEquals(1, FSession.FindAll<TFDCustomer>.Count);
  CheckEquals('Foo', FSession.FindAll<TFDCustomer>.First.Name);
end;

procedure TFireDACSessionTest.WhenSavingInTransaction_RollbackIsSuccessful;
var
  customer: TFDCustomer;
  transaction: IDBTransaction;
begin
  customer := CreateCustomer('Foo', 25);

  transaction := FSession.BeginTransaction;
  FSession.Save(customer);
  customer.Free;

  CheckEquals('Foo', FSession.FindAll<TFDCustomer>.First.Name);

  transaction.Rollback;
  CheckEquals(0, FSession.FindAll<TFDCustomer>.Count);
end;

{$ENDREGION}


initialization
  RegisterTests('Spring.Persistence.Adapters', [
    TFireDACExceptionHandlerTest.Suite,
    TFireDACConnectionAdapterTest.Suite,
    TFireDACTransactionAdapterTest.Suite,
    TFireDACStatementAdapterTest.Suite,
    TFireDACSessionTest.Suite
  ]);

end.
