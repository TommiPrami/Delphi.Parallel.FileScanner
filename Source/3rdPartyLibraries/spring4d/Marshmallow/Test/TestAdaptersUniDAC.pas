unit TestAdaptersUniDAC;

interface

uses
  Classes,
  Data.DB,
  Uni,
  Spring,
  Spring.Mocking,
  Spring.Persistence.Adapters.UniDAC,
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
  TestUniDACConnection,
  TestFramework
  ;

type
  TUniDACExceptionHandlerAccess = class(TUniDACExceptionHandler);
  TUniDACExceptionHandlerTest = class(TTestCase<TUniDACExceptionHandlerAccess>)
  published
    procedure TestGetAdapterException_EDatabaseError;
    procedure TestGetAdapterException_Others_Return_Nil;
  end;

  TBaseUniDACConnectionAdapterTest = class(TTestCase)
  strict protected
    fConnection: IDBConnection;
    fMockConnection: Mock<TTestUniConnection>;
    fSqliteConnection: TUniConnection;
    procedure CreateSqliteConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TUniDACConnectionAdapterTest = class(TBaseUniDACConnectionAdapterTest)
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

  TUniDACTransactionAdapterTest = class(TBaseUniDACConnectionAdapterTest)
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
    procedure TestDestroy_Will_Free_fTrancastion_On_Rollback_Exception;
  end;

  TUniDACStatementAdapterTest = class(TBaseUniDACConnectionAdapterTest)
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
  TUniCustomer = class
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

  TUniDACSessionTest = class(TTestCase)
  private
    FConnection: IDBConnection;
    FSession: TSession;
    UniACConnection: TUniConnection;
  protected
    procedure CreateTables;
    function CreateCustomer(const name: string; const age: Integer): TUniCustomer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Save;
    procedure SaveNullable;
    procedure WhenSavingInTransaction_RollbackIsSuccessful;
    procedure WhenSavingInTransaction_CommitIsSuccessful;
  end;

implementation

uses
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.SQL.Interfaces,
  Spring.Collections,
  TestSession,
  Variants, SQLiteUniProvider;


{$REGION 'TUniDACExceptionHandlerTest'}

procedure TUniDACExceptionHandlerTest.TestGetAdapterException_EDatabaseError;
var
  exc, result: Shared<Exception>;
begin
  exc := EDatabaseError.Create('');
  result := SUT.GetAdapterException(exc, 'message');
  CheckIs(result, EUniDACAdapterException);
  CheckEqualsString('message', result.Value.Message);
  CheckFalse(EUniDACAdapterException(result.Value).ErrorCode.HasValue);
end;

procedure TUniDACExceptionHandlerTest.TestGetAdapterException_Others_Return_Nil;
var
  exc, result: Shared<Exception>;
begin
  exc := Exception.Create('');
  result := SUT.GetAdapterException(exc, '');
  CheckNull(result);
end;

{$ENDREGION}


{$REGION 'TBaseUniDACConnectionAdapterTest'}

procedure TBaseUniDACConnectionAdapterTest.CreateSqliteConnection;
begin
  fSqliteConnection := TUniConnection.Create(nil);
  fSqliteConnection.ProviderName := 'SQLite';
  fConnection := TUniDACConnectionAdapter.Create(fSqliteConnection);
  fConnection.AutoFreeConnection := True;
end;

procedure TBaseUniDACConnectionAdapterTest.SetUp;
begin
  inherited;
  fMockConnection := Mock<TTestUniConnection>.Create(TMockBehavior.Strict);
  fConnection := TUniDACConnectionAdapter.Create(fMockConnection);
  fConnection.AutoFreeConnection := False;
end;

procedure TBaseUniDACConnectionAdapterTest.TearDown;
begin
  fConnection := nil;
  fMockConnection.Setup.Executes.When(Args.Any).SetConnected(False);
  fMockConnection.Free;
  fSqliteConnection := nil;
  inherited;
end;

{$ENDREGION}


{$REGION 'TUniDACConnectionAdapterTest'}

procedure TUniDACConnectionAdapterTest.SetUp;
begin
  inherited;

end;

procedure TUniDACConnectionAdapterTest.TestBeginTransaction;
var
  transaction: IDBTransaction;
begin
  // Test connect exception

  fMockConnection.Setup.Raises<EUniError>.When.SetConnected(True);
  CheckException(EUniDACAdapterException,
    procedure begin fConnection.BeginTransaction end);

  CreateSqliteConnection;
  transaction := fConnection.BeginTransaction;
  CheckNotNull(transaction);

  CheckException(EUniDACAdapterException,
    procedure begin fConnection.BeginTransaction end);
end;

procedure TUniDACConnectionAdapterTest.TestConnect;
begin
  fMockConnection.Setup.Executes.When.SetConnected(True);
  fConnection.Connect;
  fMockConnection.Received(1).SetConnected(True);

  Pass;
end;

procedure TUniDACConnectionAdapterTest.TestConnectException;
var
  adapter: TUniDACConnectionAdapter;
begin
  // Use native connection for this test
  adapter := TUniDACConnectionAdapter.Create(TUniConnection.Create(nil));
  try
    adapter.AutoFreeConnection := True;
    ExpectedException := EUniDACAdapterException;
    adapter.Connect;
  finally
    adapter.Free;
  end;
end;

procedure TUniDACConnectionAdapterTest.TestCreateStatement;
var
  returnValue: IDBStatement;
begin
  CreateSqliteConnection;

  returnValue := fConnection.CreateStatement;

  CheckNotNull(returnValue);
end;

procedure TUniDACConnectionAdapterTest.TestDisconnect;
begin
  fMockConnection.Setup.Raises<EUniError>.When.SetConnected(False);
  CheckException(EUniDACAdapterException,
    procedure begin fConnection.Disconnect end);

  fMockConnection.Setup.Executes.When.SetConnected(False);
  fConnection.Disconnect;
  fMockConnection.Received(2).SetConnected(False);

  Pass;
end;

procedure TUniDACConnectionAdapterTest.TestIsConnected;
begin
  fMockConnection.Setup.Returns<Boolean>(False).When.GetConnected;
  CheckFalse(fConnection.IsConnected);

  fMockConnection.Setup.Returns<Boolean>(True).When.GetConnected;
  CheckTrue(fConnection.IsConnected);
end;

{$ENDREGION}


{$REGION 'TUniDACTransactionAdapterTest'}

procedure TUniDACTransactionAdapterTest.SetUp;
begin
  inherited;
  CreateSqliteConnection;
  fTransaction := fConnection.BeginTransaction;
end;

procedure TUniDACTransactionAdapterTest.TearDown;
begin
  fTransaction := nil;
  inherited;
end;

procedure TUniDACTransactionAdapterTest.TestCommit;
begin
  fTransaction.Commit;
  Pass;
end;

procedure TUniDACTransactionAdapterTest.TestCommit_Exception;
begin
  fSqliteConnection.Connected := False;
  ExpectedException := EUniDACAdapterException;
  fTransaction.Commit;
end;

type
  TMockedUniDACTransactionAdapter = class(TUniDACTransactionAdapter)
  private
    fInTransaction: Boolean;
  public
    function InTransaction: Boolean; override;
  end;

function TMockedUniDACTransactionAdapter.InTransaction: Boolean;
begin
  // We need to return true to let the Rollback fail since we closed the
  // connection.
  Result := fInTransaction;
end;

procedure TUniDACTransactionAdapterTest.TestDestroy_Will_Free_fTrancastion_On_Rollback_Exception;
const
  objDestroyingFlag = Integer($80000000);
  objDisposedFlag = Integer($40000000);
var
  lTransaction: TUniTransaction;
{$IFDEF AUTOREFCOUNT}
  [Unsafe]
{$ENDIF}
  lInternalTransaction: TMockedUniDACTransactionAdapter;
begin
  fTransaction := nil;
  lTransaction := TUniTransaction.Create(nil);
  lTransaction.Name := 'someName';
  lTransaction.DefaultConnection := fSqliteConnection;
  lTransaction.StartTransaction;
  lInternalTransaction := TMockedUniDACTransactionAdapter.Create(lTransaction,
    TUniDACExceptionHandler.Create, True);
  lInternalTransaction.fInTransaction := True;
  fTransaction := lInternalTransaction;
  fSqliteConnection.Connected := False;
  CheckException(EUniDACAdapterException,
    procedure begin fTransaction := nil; end);
  lInternalTransaction.fInTransaction := False;
{$IFNDEF AUTOREFCOUNT}
  // CleanupInstance should clear the name if freed properly
  CheckEquals('', lTransaction.Name);
{$ELSE}
  CheckEquals(objDisposedFlag or 1, lTransaction.RefCount);
{$ENDIF}
  lInternalTransaction.FreeInstance; // Prevent memory leak
end;

procedure TUniDACTransactionAdapterTest.TestRollback;
begin
  fTransaction.Rollback;
  Pass;
end;

procedure TUniDACTransactionAdapterTest.TestRollback_Exception;
begin
  fSqliteConnection.Connected := False;
  ExpectedException := EUniDACAdapterException;
  fTransaction.Rollback;
end;

{$ENDREGION}


{$REGION 'TUniDACStatementAdapterTest'}

procedure TUniDACStatementAdapterTest.SetUp;
begin
  inherited;
  CreateSqliteConnection;
  fStatement := fConnection.CreateStatement;
end;

procedure TUniDACStatementAdapterTest.TearDown;
begin
  fStatement := nil;
  inherited;
end;

procedure TUniDACStatementAdapterTest.TestExecuteQuery_Exception;
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
      CheckIs(E, EUniDACAdapterException);
      CheckNotNull(E.InnerException);
      CheckEqualsString(Format(SCannotOpenQuery,
        [E.InnerException.Message]), E.Message);
      resultSet := nil;
    end;
  end;
end;

procedure TUniDACStatementAdapterTest.TestExecute_Exception;
begin
  fSqliteConnection.Connected := False;
  ExpectedException := EUniDACAdapterException;
  fStatement.Execute;
end;

{$ENDREGION}


{$REGION 'TUniDACSessionTest'}

function TUniDACSessionTest.CreateCustomer(const name: string;
  const age: Integer): TUniCustomer;
begin
  Result := TUniCustomer.Create;
  Result.Name := name;
  Result.Age := age;
end;

procedure TUniDACSessionTest.CreateTables;
begin
  UniACConnection.ExecSQL('CREATE TABLE IF NOT EXISTS CUSTOMERS ([ID] INTEGER PRIMARY KEY, [AGE] INTEGER NULL,'+
    '[NAME] NVARCHAR (255), [HEIGHT] FLOAT, [PICTURE] BLOB); ');
end;

procedure TUniDACSessionTest.Save;
var
  customer: TUniCustomer;
begin
  customer := CreateCustomer('Foo', 25);

  FSession.Save(customer);

  CheckEquals('Foo', FSession.FindAll<TUniCustomer>.First.Name);
  CheckFalse(FSession.FindAll<TUniCustomer>.First.Height.HasValue);
  customer.Free;
end;

procedure TUniDACSessionTest.SaveNullable;
const
  OnePointOne: Double = 1.1;
var
  customer: TUniCustomer;
begin
  customer := CreateCustomer('Foo', 25);
  customer.Height := OnePointOne;
  FSession.Save(customer);

  CheckEquals(OnePointOne, FSession.FindAll<TUniCustomer>.First.Height);
  customer.Free;
end;

procedure TUniDACSessionTest.SetUp;
begin
  UniACConnection := TUniConnection.Create(nil);
  UniACConnection.ProviderName := 'SQLite';
 // UniACConnection.Params.Add('Database=file::memory:?cache=shared');
 // UniACConnection.Params.Add('Database=:memory:');
//  inherited SetUp;
  FConnection := TConnectionFactory.GetInstance(dtUniDAC, UniACConnection);
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

procedure TUniDACSessionTest.TearDown;
begin
  inherited TearDown;
  FSession.Free;
  UniACConnection := nil;
  FConnection := nil;
end;

procedure TUniDACSessionTest.WhenSavingInTransaction_CommitIsSuccessful;
var
  customer: TUniCustomer;
  transaction: IDBTransaction;
begin
  customer := CreateCustomer('Foo', 25);

  transaction := FSession.BeginTransaction;
  FSession.Save(customer);
  customer.Free;

  CheckEquals('Foo', FSession.FindAll<TUniCustomer>.First.Name);

  transaction.Commit;
  CheckEquals(1, FSession.FindAll<TUniCustomer>.Count);
  CheckEquals('Foo', FSession.FindAll<TUniCustomer>.First.Name);
end;

procedure TUniDACSessionTest.WhenSavingInTransaction_RollbackIsSuccessful;
var
  customer: TUniCustomer;
  transaction: IDBTransaction;
begin
  customer := CreateCustomer('Foo', 25);

  transaction := FSession.BeginTransaction;
  FSession.Save(customer);
  customer.Free;

  CheckEquals('Foo', FSession.FindAll<TUniCustomer>.First.Name);

  transaction.Rollback;
  CheckEquals(0, FSession.FindAll<TUniCustomer>.Count);
end;

{$ENDREGION}


initialization
  RegisterTests('Spring.Persistence.Adapters', [
    TUniDACExceptionHandlerTest.Suite,
    TUniDACConnectionAdapterTest.Suite,
    TUniDACTransactionAdapterTest.Suite,
    TUniDACStatementAdapterTest.Suite,
    TUniDACSessionTest.Suite
  ]);

end.
