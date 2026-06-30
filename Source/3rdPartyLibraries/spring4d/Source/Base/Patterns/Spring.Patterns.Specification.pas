{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2026 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Patterns.Specification;

interface

uses
  Spring;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type

  /// <summary>
  ///   Defines the core methods of a specification interface.
  /// </summary>
  /// <remarks>
  ///   This interface is designed to be binary compatible with
  ///   Spring.Predicate&lt;T&gt;.
  /// </remarks>
  ISpecification<T> = interface(IInvokable) //FI:W524
    ['{95E8259B-1397-4A66-9E12-A734E97C1C7C}']
    function IsSatisfiedBy(const item: T): Boolean;
    // DO NOT ADD ANY METHODS HERE!!!
  end;

  /// <summary>
  ///   Provides the easy-going specification holder with operator overloads.
  /// </summary>
  Specification<T> = record
  private
    fInstance: ISpecification<T>;
  public
    function IsSatisfiedBy(const item: T): Boolean; inline;

    class operator Implicit(const specification: ISpecification<T>): Specification<T>;
    class operator Implicit(const specification: Predicate<T>): Specification<T>;
    class operator Implicit(const specification: Specification<T>): ISpecification<T>;
    class operator Implicit(const specification: Specification<T>): Predicate<T>;
    class operator Explicit(const specification: ISpecification<T>): Specification<T>;
    class operator Explicit(const specification: Predicate<T>): Specification<T>;
    class operator Explicit(const specification: Specification<T>): ISpecification<T>;

    class operator LogicalAnd(const left, right: Specification<T>): Specification<T>;
    class operator LogicalOr(const left, right: Specification<T>): Specification<T>;
    class operator LogicalNot(const value: Specification<T>): Specification<T>;

    class operator In(const left: T; const right: Specification<T>): Boolean; inline;
  end;

  /// <summary>
  ///   Provides the abstract base class for ISpecification&lt;T&gt;.
  /// </summary>
  TSpecification<T> = class abstract(TInterfacedObject, ISpecification<T>, Predicate<T>)
  protected
    function Predicate<T>.Invoke = IsSatisfiedBy;
    function IsSatisfiedBy(const item: T): Boolean; virtual; abstract;
  end;

  TUnarySpecification<T> = class abstract(TSpecification<T>)
  protected
    fValue: ISpecification<T>;
  public
    constructor Create(const value: ISpecification<T>);
  end;

  TBinarySpecification<T> = class abstract(TSpecification<T>)
  protected
    fLeft: ISpecification<T>;
    fRight: ISpecification<T>;
  public
    constructor Create(const left, right: ISpecification<T>);
  end;

  TLogicalNotSpecification<T> = class sealed(TRefCountedObject, ISpecification<T>)
  protected
    fValue: ISpecification<T>;
  protected
    function IsSatisfiedBy(const item: T): Boolean;
  public
    constructor Create(const value: ISpecification<T>);
  end;

  TLogicalAndSpecification<T> = class sealed(TRefCountedObject, ISpecification<T>)
  protected
    fLeft: ISpecification<T>;
    fRight: ISpecification<T>;
  protected
    function IsSatisfiedBy(const item: T): Boolean;
  public
    constructor Create(const left, right: ISpecification<T>);
  end;

  TLogicalOrSpecification<T> = class sealed(TRefCountedObject, ISpecification<T>)
  protected
    fLeft: ISpecification<T>;
    fRight: ISpecification<T>;
  protected
    function IsSatisfiedBy(const item: T): Boolean;
  public
    constructor Create(const left, right: ISpecification<T>);
  end;

  TSpecificationHelper = record
    class procedure LogicalAnd_Object(const left, right: IInterface; var result); static;
    class procedure LogicalOr_Object(const left, right: IInterface; var result); static;
    class procedure LogicalNot_Object(const value: IInterface; var result); static;
    class procedure LogicalAnd_Interface(const left, right: IInterface; var result); static;
    class procedure LogicalOr_Interface(const left, right: IInterface; var result); static;
    class procedure LogicalNot_Interface(const value: IInterface; var result); static;
  end;

implementation


{$REGION 'Specification<T>'}

function Specification<T>.IsSatisfiedBy(const item: T): Boolean;
begin
  Result := Assigned(fInstance) and fInstance.IsSatisfiedBy(item);
end;

class operator Specification<T>.Implicit(
  const specification: ISpecification<T>): Specification<T>;
begin
  Result.fInstance := specification;
end;

class operator Specification<T>.Implicit(
  const specification: Predicate<T>): Specification<T>;
begin
  Predicate<T>(Result.fInstance) := specification;
end;

class operator Specification<T>.Implicit(
  const specification: Specification<T>): ISpecification<T>;
begin
  Result := specification.fInstance;
end;

class operator Specification<T>.Implicit(
  const specification: Specification<T>): Predicate<T>;
begin
  Result := Predicate<T>(specification.fInstance);
end;

class operator Specification<T>.In(const left: T;
  const right: Specification<T>): Boolean;
begin
  Result := right.IsSatisfiedBy(left);
end;

class operator Specification<T>.Explicit(
  const specification: ISpecification<T>): Specification<T>;
begin
  Result.fInstance := specification;
end;

class operator Specification<T>.Explicit(
  const specification: Predicate<T>): Specification<T>;
begin
  Predicate<T>(Result.fInstance) := specification;
end;

class operator Specification<T>.Explicit(
  const specification: Specification<T>): ISpecification<T>;
begin
  Result := specification.fInstance;
end;

class operator Specification<T>.LogicalAnd(const left,
  right: Specification<T>): Specification<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: TSpecificationHelper.LogicalAnd_Object(
      left.fInstance, right.fInstance, Result.fInstance);
    tkInterface: TSpecificationHelper.LogicalAnd_Interface(
      left.fInstance, right.fInstance, Result.fInstance);
  else{$ELSE}begin{$ENDIF}
    Result.fInstance := TLogicalAndSpecification<T>.Create(
      left.fInstance, right.fInstance);
  end;
end;

class operator Specification<T>.LogicalOr(const left,
  right: Specification<T>): Specification<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: TSpecificationHelper.LogicalOr_Object(
      left.fInstance, right.fInstance, Result.fInstance);
    tkInterface: TSpecificationHelper.LogicalOr_Interface(
      left.fInstance, right.fInstance, Result.fInstance);
  else{$ELSE}begin{$ENDIF}
    Result.fInstance := TLogicalOrSpecification<T>.Create(
      left.fInstance, right.fInstance);
  end;
end;

class operator Specification<T>.LogicalNot(
  const value: Specification<T>): Specification<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: TSpecificationHelper.LogicalNot_Object(
      value.fInstance, Result.fInstance);
    tkInterface: TSpecificationHelper.LogicalNot_Interface(
      value.fInstance, Result.fInstance);
  else{$ELSE}begin{$ENDIF}
    Result.fInstance := TLogicalNotSpecification<T>.Create(
      value.fInstance);
  end;
end;

{$ENDREGION}


{$REGION 'TUnarySpecification<T>'}

constructor TUnarySpecification<T>.Create(const value: ISpecification<T>);
begin
  fValue := value;
end;

{$ENDREGION}


{$REGION 'TBinarySpecification<T>'}

constructor TBinarySpecification<T>.Create(const left, right: ISpecification<T>);
begin
  fLeft := left;
  fRight := right;
end;

{$ENDREGION}


{$REGION 'TLogicalNotSpecification<T>'}

constructor TLogicalNotSpecification<T>.Create(const value: ISpecification<T>);
begin
  fValue := value;
end;

function TLogicalNotSpecification<T>.IsSatisfiedBy(const item: T): Boolean;
begin
  Result := not fValue.IsSatisfiedBy(item);
end;

{$ENDREGION}


{$REGION 'TLogicalAndSpecification<T>'}

constructor TLogicalAndSpecification<T>.Create(const left, right: ISpecification<T>);
begin
  fLeft := left;
  fRight := right;
end;

function TLogicalAndSpecification<T>.IsSatisfiedBy(const item: T): Boolean;
begin
  Result := fLeft.IsSatisfiedBy(item) and fRight.IsSatisfiedBy(item);
end;

{$ENDREGION}


{$REGION 'TLogicalOrSpecification<T>'}

constructor TLogicalOrSpecification<T>.Create(const left, right: ISpecification<T>);
begin
  fLeft := left;
  fRight := right;
end;

function TLogicalOrSpecification<T>.IsSatisfiedBy(const item: T): Boolean;
begin
  Result := fLeft.IsSatisfiedBy(item) or fRight.IsSatisfiedBy(item);
end;

{$ENDREGION}


{$REGION 'TSpecificationHelper'}

class procedure TSpecificationHelper.LogicalAnd_Object(const left, right: IInterface; var result);
begin
  ISpecification<TObject>(result) := TLogicalAndSpecification<TObject>.Create(
    ISpecification<TObject>(left), ISpecification<TObject>(right));
end;

class procedure TSpecificationHelper.LogicalOr_Object(const left, right: IInterface; var result);
begin
  ISpecification<TObject>(result) := TLogicalOrSpecification<TObject>.Create(
    ISpecification<TObject>(left), ISpecification<TObject>(right));
end;

class procedure TSpecificationHelper.LogicalNot_Object(const value: IInterface;
  var result);
begin
  ISpecification<TObject>(result) := TLogicalNotSpecification<TObject>.Create(
    ISpecification<TObject>(value));
end;

class procedure TSpecificationHelper.LogicalAnd_Interface(const left, right: IInterface; var result);
begin
  ISpecification<IInterface>(result) := TLogicalAndSpecification<IInterface>.Create(
    ISpecification<IInterface>(left), ISpecification<IInterface>(right));
end;

class procedure TSpecificationHelper.LogicalOr_Interface(const left, right: IInterface; var result);
begin
  ISpecification<IInterface>(result) := TLogicalOrSpecification<IInterface>.Create(
    ISpecification<IInterface>(left), ISpecification<IInterface>(right));
end;

class procedure TSpecificationHelper.LogicalNot_Interface(
  const value: IInterface; var result);
begin
  ISpecification<IInterface>(result) := TLogicalNotSpecification<IInterface>.Create(
    ISpecification<IInterface>(value));
end;

{$ENDREGION}


end.
