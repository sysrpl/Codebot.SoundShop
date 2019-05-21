unit Delegates;

{$i defaults.inc}

interface

type
  Float = Single;
  LargeWord = QWord;

  TArray<T> = array of T;
  TCompare<T> = function(constref A, B: T): Integer;
  TConvertString<TItem> = function(constref Item: TItem): string;
  TFilterFunc<T> = function(constref Value: T): Boolean;

{ TArrayEnumerator<T> }

  TArrayEnumerator<T> = class(TInterfacedObject, IEnumerator<T>)
  private
    FItems: TArray<T>;
    FPosition: Integer;
    FCount: Integer;
  public
    constructor Create(Items: TArray<T>; Count: Integer = -1);
    { IEnumerator<T> }
    function GetCurrent: T;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: T read GetCurrent;
  end;

{ TSortingOrder can be used to a sort items forward, backwards, or not at all }

  TSortingOrder = (soAscend, soDescend, soNone);

{ TArrayList<T> is a simple extension to dynamic arrays }

  TArrayList<T> = record
  public
  type
    TArrayListEnumerator = class(TArrayEnumerator<T>) end;
    TCompareFunc = TCompare<T>;
    { Get the enumerator for the list }
    function GetEnumerator: IEnumerator<T>;
  private
    function CompareExists: Boolean;
    procedure QuickSort(Order: TSortingOrder; Compare: TCompare<T>; L, R: Integer);
    function GetIsEmpty: Boolean;
    function GetFirst: T;
    procedure SetFirst(const Value: T);
    function GetLast: T;
    procedure SetLast(const Value: T);
    function GetLength: Integer;
    procedure SetLength(Value: Integer);
    function GetData: Pointer;
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
  public
    { The array acting as a list }
    Items: TArray<T>;
    class var DefaultCompare: TCompare<T>;
    class var DefaultConvertString: TConvertString<T>;
    class function ArrayOf(const Items: array of T): TArrayList<T>; static;
    class function Convert: TArrayList<T>; static;
    { Convert a list to an array }
    class operator Implicit(const Value: TArrayList<T>): TArray<T>;
    { Convert an array to a list }
    class operator Implicit(const Value: TArray<T>): TArrayList<T>;
    { Convert an open array to a list }
    class operator Implicit(const Value: array of T): TArrayList<T>;
    { Performs a simple safe copy of up to N elements }
    procedure Copy(out List: TArrayList<T>; N: Integer);
    { Performs a fast unsafe copy of up to N elements }
    procedure CopyFast(out List: TArrayList<T>; N: Integer);
    { Returns the lower bounds of the list }
    function Lo: Integer;
    { Returns the upper bounds of the list }
    function Hi: Integer;
    { Reverses theitems in the list }
    procedure Reverse;
    { Swap two items in the list }
    procedure Exchange(A, B: Integer);
    { Adds and item to the end of the list }
    procedure Push(const Item: T);
    { Appends an array of items to the list }
    procedure PushRange(const Collection: array of T);
    { Remove an item from the end of the list }
    function Pop: T;
    { Remove an item randomly from the list }
    function PopRandom: T;
    { Return a copy of the list with items passing through a filter }
    function Filter(Func: TFilterFunc<T>): TArrayList<T>;
    { Resurn the first item matching a condition }
    function FirstOf(Func: TFilterFunc<T>): T;
    { Removes an item by index from the list and decresaes the count by one }
    procedure Delete(Index: Integer);
    { Removes all items setting the count of the list to 0 }
    procedure Clear;
    { Sort the items using a comparer }
    procedure Sort(Order: TSortingOrder = soAscend; Comparer: TCompare<T> = nil);
    { Attempt to find the item using DefaultCompare }
    function IndexOf(const Item: T): Integer; overload;
    { Attempt to find the item using a supplied comparer }
    function IndexOf(const Item: T; Comparer: TCompare<T>): Integer; overload;
    { Join a the array into a string using a separator }
    function Join(const Separator: string; Convert: TConvertString<T> = nil): string;
    { Returns true if ther are no items in the list }
    property IsEmpty: Boolean read GetIsEmpty;
    { First item in the list }
    property First: T read GetFirst write SetFirst;
    { Last item in the list }
    property Last: T read GetLast write SetLast;
    { Number of items in the list }
    property Length: Integer read GetLength write SetLength;
    { Address where to the first item is located }
    property Data: Pointer read GetData;
    { Get or set an item }
    property Item[Index: Integer]: T read GetItem write SetItem; default;
  end;

{ IDelegate<T> }

  IDelegate<T> = interface
  ['{ADBC29C1-4F3D-4E4C-9A79-C805E8B9BD92}']
    { Check if there are no subscribers }
    function GetIsEmpty: Boolean;
    { A subscriber calls add to register an event handler }
    procedure Add(const Handler: T);
    { A subscriber calls remove to unregister an event handler }
    procedure Remove(const Handler: T);
    { Empty is true when there are no subscribers }
    property IsEmpty: Boolean read GetIsEmpty;
  end;

{ IDelegateContainer<T> }

  IDelegateContainer<T> = interface
  ['{ED255F00-3112-4315-9E25-3C1B3064C932}']
    function GetEnumerator: IEnumerator<T>;
    function GetDelegate: IDelegate<T> ;
    property Delegate: IDelegate<T> read GetDelegate;
  end;

{ TDelegateImpl<T> }

  TDelegateImpl<T> = class(TInterfacedObject, IDelegate<T>)
  private
    FList: TArrayList<T>;
    function IndexOf(Event: T): Integer;
  protected
    function GetIsEmpty: Boolean;
    procedure Add(const Event: T);
    procedure Remove(const Event: T);
  end;

  TDelegateContainerImpl<T> = class(TInterfacedObject, IDelegateContainer<T>)
  private
    type TDelegateClass = TDelegateImpl<T>;
    var FDelegateClass: TDelegateClass;
    var FDelegate: IDelegate<T>;
  protected
    { IDelegateContainer<T> }
    function GetEnumerator: IEnumerator<T>;
    function GetDelegate: IDelegate<T>;
  end;

{ TDelegate<T> }

  TDelegate<T> = record
  private
    type TDelegateContainer = TDelegateContainerImpl<T>;
    var FContainer: IDelegateContainer<T>;
    function GetContainer: IDelegateContainer<T>;
  public
    { Convert a delegate into an interface suitable for subscribers }
    class operator Implicit(var Delegate: TDelegate<T>): IDelegate<T>;
    { Get the enumerator of subscriber's events }
    function GetEnumerator: IEnumerator<T>;
    { Check is there are no subscribers }
    function GetIsEmpty: Boolean;
    { Add an event handler }
    procedure Add(const Handler: T);
    { Remove an event handler }
    procedure Remove(const Handler: T);
    { Returns true is there a no subscribers }
    property IsEmpty: Boolean read GetIsEmpty;
  end;

{ Examples of how to use delegates:

  TNotifyDelegate = TDelegate<TNotifyEvent>;
  INotifyDelegate = IDelegate<TNotifyEvent>;
  TMethodEvent = procedure of object;
  TMethodDelegate = TDelegate<TMethodEvent>;
  IMethodDelegate = IDelegate<TMethodEvent>; }

function MemCompare(const A, B; Size: LongWord): Boolean;
function StrCompare(const A, B: string): Integer;
function Int64ToStr(Value: Int64): string;

function DefaultCompare8(constref A, B: Byte): Integer;
function DefaultCompare16(constref A, B: Word): Integer;
function DefaultCompare32(constref A, B: LongWord): Integer;
function DefaultCompare64(constref A, B: LargeWord): Integer;
function DefaultStringCompare(constref A, B: string): Integer;
function DefaultStringConvertString(constref Item: string): string;
function DefaultWordCompare(constref A, B: Word): Integer;
function DefaultWordConvertString(constref Item: Word): string;
function DefaultIntCompare(constref A, B: Integer): Integer;
function DefaultIntConvertString(constref Item: Integer): string;
function DefaultInt64Compare(constref A, B: Int64): Integer;
function DefaultInt64ConvertString(constref Item: Int64): string;
function DefaultFloatCompare(constref A, B: Float): Integer;
function DefaultFloatConvertString(constref Item: Float): string;
function DefaultObjectCompare(constref A, B: TObject): Integer;
function DefaultInterfaceCompare(constref A, B: IInterface): Integer;

type
  StringArray = TArrayList<string>;
  WordArray = TArrayList<Word>;
  IntArray = TArrayList<Integer>;
  Int64Array = TArrayList<Int64>;
  FloatArray = TArrayList<Float>;

implementation

{ TArrayEnumerator<T> }

constructor TArrayEnumerator<T>.Create(Items: TArray<T>; Count: Integer = -1);
begin
  inherited Create;
  FItems := Items;
  FPosition := -1;
  if Count < 0 then
    FCount := Length(Items)
  else
    FCount := Count;
end;

function TArrayEnumerator<T>.GetCurrent: T;
begin
  Result := FItems[FPosition];
end;

function TArrayEnumerator<T>.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FCount;
end;

procedure TArrayEnumerator<T>.Reset;
begin
  FPosition := -1;
end;

{ TArrayList<T> }

function TArrayList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TArrayListEnumerator.Create(Items);
end;

class operator TArrayList<T>.Implicit(const Value: TArrayList<T>): TArray<T>;
begin
  Result := Value.Items;
end;

class operator TArrayList<T>.Implicit(const Value: TArray<T>): TArrayList<T>;
begin
  Result.Items := Value;
end;

class operator TArrayList<T>.Implicit(const Value: array of T): TArrayList<T>;
var
  I: T;
begin
  for I in Value do
    Result.Push(I);
end;

class function TArrayList<T>.ArrayOf(const Items: array of T): TArrayList<T>;
var
  I: T;
begin
  for I in Items do
    Result.Push(I);
end;

procedure TArrayList<T>.Copy(out List: TArrayList<T>; N: Integer);
var
  I: Integer;
begin
  if N < 1 then
    N := Length
  else if N > Length then
    N := Length;
  List.Length := N;
  if N < 1 then
    Exit;
  for I := 0 to N - 1 do
    List.Items[I] := Items[I];
end;

procedure TArrayList<T>.CopyFast(out List: TArrayList<T>; N: Integer);
begin
  if N < 1 then
    N := Length
  else if N > Length then
    N := Length;
  List.Length := N;
  if N < 1 then
    Exit;
  System.Move(Items[0], List.Items[0], N * SizeOf(T));
end;

procedure TArrayList<T>.Reverse;
var
  Swap: T;
  I, J: Integer;
begin
  I := 0;
  J := Length;
  while I < J do
  begin
    Swap := Items[I];
    Items[I] := Items[J];
    Items[J] := Swap;
    Inc(I);
    Dec(J);
  end;
end;

function TArrayList<T>.Lo: Integer;
begin
  Result := Low(Items);
end;

function TArrayList<T>.Hi: Integer;
begin
  Result := High(Items);
end;

procedure TArrayList<T>.Exchange(A, B: Integer);
var
  Item: T;
begin
  if A <> B then
  begin
    Item := Items[A];
    Items[A] := Items[B];
    Items[B] := Item;
  end;
end;

procedure TArrayList<T>.Push(const Item: T);
var
  I: Integer;
begin
  I := Length;
  Length := I + 1;
  Items[I] := Item;
end;

procedure TArrayList<T>.PushRange(const Collection: array of T);
var
  I, J: Integer;
begin
  I := Length;
  J := High(Collection) - Low(Collection) + 1;
  if J < 1 then
    Exit;
  Length := I + J;
  for J := Low(Collection) to High(Collection) do
  begin
    Items[I] := Collection[J];
    Inc(I);
  end;
end;

function TArrayList<T>.Pop: T;
var
  I: Integer;
begin
  I := Length - 1;
  if I < 0 then
  begin
    Result := Default(T);
    Length := 0;
  end
  else
  begin
    Result := Items[I];
    Length := I;
  end;
end;

function TArrayList<T>.PopRandom: T;
var
  I: Integer;
begin
  I := Length;
  if I < 2 then
    Result := Pop
  else
  begin
    I := System.Random(I);
    Result := Items[I];
    Delete(I);
  end;
end;

function TArrayList<T>.Filter(Func: TFilterFunc<T>): TArrayList<T>;
var
  I, J: Integer;
begin
  J := System.Length(Items);
  System.SetLength(Result.Items, J);
  J := 0;
  for I := 0 to System.Length(Items) - 1 do
    if Func(Items[I]) then
    begin
   Result.Items[J] := Items[I];
   Inc(J);
    end;
  System.SetLength(Result.Items, J);
end;

function TArrayList<T>.FirstOf(Func: TFilterFunc<T>): T;
var
  I: Integer;
begin
  for I := 0 to System.Length(Items) - 1 do
    if Func(Items[I]) then
   Exit(Items[I]);
  Result := Default(T);
end;

procedure TArrayList<T>.Delete(Index: Integer);
var
  I, J: Integer;
begin
  I := Length - 1;
  for J := Index + 1 to I do
    Items[J - 1] := Items[J];
  Length := I;
end;

procedure TArrayList<T>.Clear;
begin
  Length := 0;
end;

function TArrayList<T>.CompareExists: Boolean;
begin
  if Assigned(DefaultCompare) then
    Exit(True);
  case SizeOf(T) of
    8: DefaultCompare := TCompareFunc(DefaultCompare8);
    16: DefaultCompare := TCompareFunc(DefaultCompare16);
    32: DefaultCompare := TCompareFunc(DefaultCompare32);
    64: DefaultCompare := TCompareFunc(DefaultCompare64);
  end;
  Result := Assigned(DefaultCompare);
end;

procedure TArrayList<T>.QuickSort(Order: TSortingOrder; Compare: TCompare<T>; L, R: Integer);
var
  F, I, J, P: Integer;
begin
  repeat
    if Order = soDescend then
   F := -1
    else
   F := 1;
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
   while Compare(Items[I], Items[P]) * F < 0 do Inc(I);
   while Compare(Items[J], Items[P]) * F > 0 do Dec(J);
   if I <= J then
   begin
  Exchange(I, J);
  if P = I then
    P := J
  else if P = J then
    P := I;
  Inc(I);
  Dec(J);
   end;
    until I > J;
    if L < J then QuickSort(Order, Compare, L, J);
    L := I;
  until I >= R;
end;

procedure TArrayList<T>.Sort(Order: TSortingOrder = soAscend; Comparer: TCompare<T> = nil);
var
  I: Integer;
begin
  if Order = soNone then
    Exit;
  I := Length;
  if I < 2 then
    Exit;
  if Assigned(Comparer) then
    QuickSort(Order, Comparer, 0, I - 1)
  else if CompareExists then
    QuickSort(Order, DefaultCompare, 0, I - 1);
end;

function TArrayList<T>.IndexOf(const Item: T): Integer;
var
  I: Integer;
begin
  Result := -1;
  I := Length;
  if (I > 0) and CompareExists then
    for I := Lo to Hi do
      if DefaultCompare(Item, Items[I]) = 0 then
  Exit(I);
end;

function TArrayList<T>.IndexOf(const Item: T; Comparer: TCompare<T>): Integer;
var
  I: Integer;
begin
  Result := -1;
  I := Length;
  if I > 0 then
    for I := Lo to Hi do
   if Comparer(Item, Items[I]) = 0 then
  Exit(I);
end;

function TArrayList<T>.Join(const Separator: string; Convert: TConvertString<T> = nil): string;
var
  I: Integer;
begin
  Result := '';
  if Length < 1 then
    Exit;
  if Assigned(Convert) then
  begin
    Result := Convert(First);
    for I := Low(Items) + 1 to High(Items) do
   Result := Result + Separator + Convert(Items[I]);
  end
  else if Assigned(DefaultConvertString) then
  begin
    Result := DefaultConvertString(First);
    for I := Low(Items) + 1 to High(Items) do
   Result := Result + Separator + DefaultConvertString(Items[I]);
  end;
end;

function TArrayList<T>.GetIsEmpty: Boolean;
begin
  Result := Length = 0;
end;

function TArrayList<T>.GetFirst: T;
begin
  if Length > 0 then
    Result := Items[0]
  else
    Result := Default(T);
end;

procedure TArrayList<T>.SetFirst(const Value: T);
begin
  if Length > 0 then
    Items[0] := Value;
end;

function TArrayList<T>.GetLast: T;
begin
  if Length > 0 then
    Result := Items[Length - 1]
  else
    Result := Default(T);
end;

procedure TArrayList<T>.SetLast(const Value: T);
begin
  if Length > 0 then
    Items[Length - 1] := Value;
end;

function TArrayList<T>.GetLength: Integer;
begin
  Result := System.Length(Items);
end;

procedure TArrayList<T>.SetLength(Value: Integer);
begin
  System.SetLength(Items, Value);
end;

function TArrayList<T>.GetData: Pointer;
begin
  Result := @Items[0];
end;

function TArrayList<T>.GetItem(Index: Integer): T;
begin
  Result := Items[Index];
end;

procedure TArrayList<T>.SetItem(Index: Integer; const Value: T);
begin
  Items[Index] := Value;
end;

class function TArrayList<T>.Convert: TArrayList<T>;
begin
  Result.Length := 0;
end;

{ TDelegateImpl<T> }

function TDelegateImpl<T>.IndexOf(Event: T): Integer;
var
  Item: T;
  I: Integer;
begin
  I := 0;
  for Item in FList do
    if MemCompare(Item, Event, SizeOf(T)) then
      Exit(I)
    else
      Inc(I);
  Result := -1;
end;

{ TDelegateImpl<T>.IDelegate<T> }

function TDelegateImpl<T>.GetIsEmpty: Boolean;
begin
  Result := FList.IsEmpty;
end;

procedure TDelegateImpl<T>.Add(const Event: T);
var
  I: Integer;
begin
  I := IndexOf(Event);
  if I < 0 then
    FList.Push(Event);
end;

procedure TDelegateImpl<T>.Remove(const Event: T);
var
  I: Integer;
begin
  I := IndexOf(Event);
  if I > -1 then
    FList.Delete(I);
end;

{ TDelegateContainerImpl<T>.IDelegateContainer<T> }

function TDelegateContainerImpl<T>.GetDelegate: IDelegate<T>;
begin
  if FDelegate = nil then
  begin
    FDelegate := TDelegateImpl<T>.Create;
    FDelegateClass := FDelegate as TDelegateClass;
  end;
  Result := FDelegate;
end;

function TDelegateContainerImpl<T>.GetEnumerator: IEnumerator<T>;
begin
  GetDelegate;
  Result := FDelegateClass.FList.GetEnumerator;
end;

{ TDelegate<T> }

class operator TDelegate<T>.Implicit(var Delegate: TDelegate<T>): IDelegate<T>;
begin
  Result := Delegate.GetContainer.Delegate;
end;

function TDelegate<T>.GetContainer: IDelegateContainer<T>;
begin
  if FContainer = nil then
    FContainer := TDelegateContainer.Create;
  Result := FContainer;
end;

function TDelegate<T>.GetEnumerator: IEnumerator<T>;
begin
  if FContainer = nil then
    FContainer := TDelegateContainer.Create;
  Result := FContainer.GetEnumerator;
end;

function TDelegate<T>.GetIsEmpty: Boolean;
begin
  Result := GetContainer.Delegate.IsEmpty;
end;

procedure TDelegate<T>.Add(const Handler: T);
begin
  GetContainer.Delegate.Add(Handler);
end;

procedure TDelegate<T>.Remove(const Handler: T);
begin
  GetContainer.Delegate.Remove(Handler);
end;

{ Compares }

function MemCompare(const A, B; Size: LongWord): Boolean;
var
  C, D: PByte;
begin
  C := @A;
  D := @B;
  if (C = nil) or (D = nil) then
    Exit(False);
  while Size > 0 do
  begin
    if C^ <> D^ then
      Exit(False);
    Inc(C);
    Inc(D);
    Dec(Size);
  end;
  Result := True;
end;

function StrCompare(const A, B: string): Integer;
var
  C, D: PChar;
begin
  if (Length(A) = 0) and (Length(B) = 0) then
    Exit(0);
  if Length(A) = 0 then
    Exit(-1);
  if Length(B) = 0 then
    Exit(1);
  C := PChar(A);
  D := PChar(B);
  while True do
  begin
    if C[0] < D[0] then
      Exit(-1);
    if C[0] > D[0] then
      Exit(1);
    if C[0] = #0 then
      Exit(0);
    Inc(C);
    Inc(D);
  end;
end;

function Int64ToStr(Value: Int64): string;
{var
  I, J: Integer;
begin
  S := '';
  I := Abs(Value);
  J := (I div 10) + 1;
  if Value < 0 then
    Inc(J);
  SetLength(Result, J);
  if Value < 0 then
  begin
    Result[1] := '-';
  end;
  repeat
    Result[J] := Chr(I mod 10 + Ord('0'));
    I := I div 10;
    Dec(J);
  until I = 0;
end;}
begin
  Str(Value, Result);
end;

function DefaultCompare8(constref A, B: Byte): Integer;
begin
  Result := B - A;
end;

function DefaultCompare16(constref A, B: Word): Integer;
begin
  Result := B - A;
end;

function DefaultCompare32(constref A, B: LongWord): Integer;
begin
  Result := B - A;
end;

function DefaultCompare64(constref A, B: LargeWord): Integer;
begin
  Result := B - A;
end;

function DefaultStringCompare(constref A, B: string): Integer;
begin
  Result := StrCompare(A, B);
end;

function DefaultStringConvertString(constref Item: string): string;
begin
  Result := Item;
end;

function DefaultWordCompare(constref A, B: Word): Integer;
begin
  Result := B - A;
end;

function DefaultWordConvertString(constref Item: Word): string;
begin
  Result := Int64ToStr(Item);
end;

function DefaultIntCompare(constref A, B: Integer): Integer;
begin
  Result := B - A;
end;

function DefaultIntConvertString(constref Item: Integer): string;
begin
  Result := Int64ToStr(Item);
end;

function DefaultInt64Compare(constref A, B: Int64): Integer;
begin
  Result := B - A;
end;

function DefaultInt64ConvertString(constref Item: Int64): string;
begin
  Result := Int64ToStr(Item);
end;

function DefaultFloatCompare(constref A, B: Float): Integer;
begin
  if A < B then
    Result := -1
  else if A > B then
    Result := 1
  else
    Result := 0;
end;

function DefaultFloatConvertString(constref Item: Float): string;
begin
  Str(Item, Result);
end;

function DefaultObjectCompare(constref A, B: TObject): Integer;
begin
  Result := IntPtr(A) - IntPtr(B);
end;

function DefaultInterfaceCompare(constref A, B: IInterface): Integer;
begin
  Result := IntPtr(A) - IntPtr(B);
end;

initialization
  StringArray.DefaultCompare := DefaultStringCompare;
  StringArray.DefaultConvertString := DefaultStringConvertString;
  WordArray.DefaultCompare := DefaultWordCompare;
  WordArray.DefaultConvertString := DefaultWordConvertString;
  IntArray.DefaultCompare := DefaultIntCompare;
  IntArray.DefaultConvertString := DefaultIntConvertString;
  Int64Array.DefaultCompare := DefaultInt64Compare;
  Int64Array.DefaultConvertString := DefaultInt64ConvertString;
  FloatArray.DefaultCompare := DefaultFloatCompare;
  FloatArray.DefaultConvertString := DefaultFloatConvertString;
end.

