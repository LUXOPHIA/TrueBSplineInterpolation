unit GenPoins;

interface //#################################################################### ■

uses LUX;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGenPoins

     TGenPoins = class
     private
       procedure MakePoins;
     protected
       _PoinMinI  :Integer;
       _PoinMaxI  :Integer;
       _PoinMinY :Single;
       _PoinMaxY :Single;
       _Verts    :TArray2<Single>;
       ///// アクセス
       function GetPoinMinI :Integer;
       procedure SetPoinMinI( const PoinMin_:Integer );
       function GetPoinMaxI :Integer;
       procedure SetPoinMaxI( const PoinMax_:Integer );
       function GetPoinMinY :Single;
       procedure SetPoinMinY( const PoinMinY_:Single );
       function GetPoinMaxY :Single;
       procedure SetPoinMaxY( const PoinMaxY_:Single );
       function GetVerts( const Y_,X_:Integer ) :Single;
       procedure SetVerts( const Y_,X_:Integer; const Verts_:Single );
     public
       constructor Create;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property PoinMinI                     :Integer read GetPoinMinI write SetPoinMinI;
       property PoinMaxI                     :Integer read GetPoinMaxI write SetPoinMaxI;
       property PoinMinY                     :Single  read GetPoinMinY write SetPoinMinY;
       property PoinMaxY                     :Single  read GetPoinMaxY write SetPoinMaxY;
       property Verts[ const X_,Y_:Integer ] :Single  read GetVerts    write SetVerts   ;
       ///// メソッド
       function Poins( const I_:Integer; const Td_:Single ) :Single;
       procedure Next;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses LUX.D4, LUX.Curve.T1.D1;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGenPoins

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

procedure TGenPoins.MakePoins;
var
   I :Integer;
   W :Single;
begin
     SetLength( _Verts, 4, PoinMaxI - PoinMinI + 1 );

     W := _PoinMaxY - _PoinMinY;

     for I := PoinMinI to PoinMaxI do
     begin
          Verts[ -1, I ] := W * Random + _PoinMinY;
          Verts[  0, I ] := W * Random + _PoinMinY;
          Verts[ +1, I ] := W * Random + _PoinMinY;
          Verts[ +2, I ] := W * Random + _PoinMinY;
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

function TGenPoins.GetPoinMinI :Integer;
begin
     Result := _PoinMinI;
end;

procedure TGenPoins.SetPoinMinI( const PoinMin_:Integer );
begin
     _PoinMinI := PoinMin_;  MakePoins;
end;

function TGenPoins.GetPoinMaxI :Integer;
begin
     Result := _PoinMaxI;
end;

procedure TGenPoins.SetPoinMaxI( const PoinMax_:Integer );
begin
     _PoinMaxI := PoinMax_;  MakePoins;
end;

//------------------------------------------------------------------------------

function TGenPoins.GetPoinMinY :Single;
begin
     Result := _PoinMinY;
end;

procedure TGenPoins.SetPoinMinY( const PoinMinY_:Single );
begin
     _PoinMinY := PoinMinY_;  MakePoins;
end;

function TGenPoins.GetPoinMaxY :Single;
begin
     Result := _PoinMaxY;
end;

procedure TGenPoins.SetPoinMaxY( const PoinMaxY_:Single );
begin
     _PoinMaxY := PoinMaxY_;  MakePoins;
end;

//------------------------------------------------------------------------------

function TGenPoins.GetVerts( const Y_,X_:Integer ) :Single;
begin
     Result := _Verts[ 1 + Y_, X_ - _PoinMinI ];
end;

procedure TGenPoins.SetVerts( const Y_,X_:Integer; const Verts_:Single );
begin
     _Verts[ 1 + Y_, X_ - _PoinMinI ] := Verts_;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TGenPoins.Create;
begin
     inherited;

end;

procedure TGenPoins.AfterConstruction;
begin
     inherited;

     PoinMinI := 0;
     PoinMaxI := 8;

     PoinMinY := -1.5;
     PoinMaxY := +1.5;
end;

destructor TGenPoins.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TGenPoins.Poins( const I_:Integer; const Td_:Single ) :Single;
begin
     Result := BSpline4( Verts[ -1, I_ ],
                         Verts[  0, I_ ],
                         Verts[ +1, I_ ],
                         Verts[ +2, I_ ], Td_ );
end;

//------------------------------------------------------------------------------

procedure TGenPoins.Next;
var
   I :Integer;
   W :Single;
begin
     W := _PoinMaxY - _PoinMinY;

     for I := PoinMinI to PoinMaxI do
     begin
          Verts[ -1, I ] := Verts[  0, I ];
          Verts[  0, I ] := Verts[ +1, I ];
          Verts[ +1, I ] := Verts[ +2, I ];
          Verts[ +2, I ] := W * Random + _PoinMinY;
     end;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■