unit Core;

interface //#################################################################### ■

uses LUX, LUX.D1;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TSingleBSInterp
     //
     //  00    01    02    03    04    05    06    07    08    09    10    11
     //  │    │    ┃    ┃    ┃    ┃    ┃    ┃    ┃    ┃    │    │
     //  ○----○----◆----●━━●━━●━━●━━●━━●----◆----○----○
     //  │    │    ┃    ┃    ┃    ┃    ┃    ┃    ┃    ┃    │    │
     // -03   -02   -01    00   +01   +02   +03   +04   +05   +06   +07   +08
     //  │          ┃    ┃                            ┃    ┃          │
     //  │          ┃<１>┃<-----------Curv----------->┃<１>┃          │
     //  │<---FW--->┃<-----------------Vert----------------->┃<---FW--->│
     //  │<-----------------------------Poin----------------------------->│

     TSingleBSInterp = class
     private
       procedure MakePoins;
     protected
       _FilterW  :Integer;
       _CurvMinI :Integer;
       _CurvMaxI :Integer;
       _Poins    :TArray<Single>;  upPoins :Boolean;
       _Verts    :TArray<Single>;
       ///// アクセス
       function GetFilterW :Integer;
       procedure SetFilterW( const FilterW_:Integer );
       function GetCurvMinI :Integer;
       procedure SetCurvMinI( const CurvMinI_:Integer );
       function GetCurvMaxI :Integer;
       procedure SetCurvMaxI( const CurvMaxI_:Integer );
       function GetVertMinI :Integer;
       function GetVertMaxI :Integer;
       function GetPoinMinI :Integer;
       function GetPoinMaxI :Integer;
       function GetPoins( const I_:Integer ) :Single;
       procedure SetPoins( const I_:Integer; const Poins_:Single );
       function GetVerts( const I_:Integer ) :Single;
       procedure SetVerts( const I_:Integer; const Verts_:Single );
       ///// メソッド
       procedure MakeVerts;
     public
       constructor Create;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property FilterW                   :Integer read GetFilterW  write SetFilterW ;
       property PoinMinI                  :Integer read GetPoinMinI                  ;
       property PoinMaxI                  :Integer read GetPoinMaxI                  ;
       property VertMinI                  :Integer read GetVertMinI                  ;
       property VertMaxI                  :Integer read GetVertMaxI                  ;
       property CurvMinI                  :Integer read GetCurvMinI write SetCurvMinI;
       property CurvMaxI                  :Integer read GetCurvMaxI write SetCurvMaxI;
       property Poins[ const I_:Integer ] :Single  read GetPoins    write SetPoins   ;
       property Verts[ const I_:Integer ] :Single  read GetVerts                     ;
       ///// メソッド
       function Curv( const X_:Single ) :Single;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.Math,
     LUX.Curve.T1.D1;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TSingleBSInterp

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

procedure TSingleBSInterp.MakePoins;
begin
     SetLength( _Poins, PoinMaxI - PoinMinI + 1     );  upPoins := True;
     SetLength( _Verts, VertMaxI - VertMinI + 1 + 1 );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

function TSingleBSInterp.GetFilterW :Integer;
begin
     Result := _FilterW;
end;

procedure TSingleBSInterp.SetFilterW( const FilterW_:Integer );
begin
     _FilterW := FilterW_;  MakePoins;
end;

//------------------------------------------------------------------------------

function TSingleBSInterp.GetCurvMinI :Integer;
begin
     Result := _CurvMinI;
end;

procedure TSingleBSInterp.SetCurvMinI( const CurvMinI_:Integer );
begin
     _CurvMinI := CurvMinI_;  MakePoins;
end;

function TSingleBSInterp.GetCurvMaxI :Integer;
begin
     Result := _CurvMaxI;
end;

procedure TSingleBSInterp.SetCurvMaxI( const CurvMaxI_:Integer );
begin
     _CurvMaxI := CurvMaxI_;  MakePoins;
end;

//------------------------------------------------------------------------------

function TSingleBSInterp.GetVertMinI :Integer;
begin
     Result := CurvMinI - 1;
end;

function TSingleBSInterp.GetVertMaxI :Integer;
begin
     Result := CurvMaxI + 1;
end;

//------------------------------------------------------------------------------

function TSingleBSInterp.GetPoinMinI :Integer;
begin
     Result := VertMinI - FilterW;
end;

function TSingleBSInterp.GetPoinMaxI :Integer;
begin
     Result := VertMaxI + FilterW;
end;

//------------------------------------------------------------------------------

function TSingleBSInterp.GetPoins( const I_:Integer ) :Single;
begin
     Result := _Poins[ I_ - PoinMinI ];
end;

procedure TSingleBSInterp.SetPoins( const I_:Integer; const Poins_:Single );
begin
     _Poins[ I_ - PoinMinI ] := Poins_;  upPoins := True;
end;

//------------------------------------------------------------------------------

function TSingleBSInterp.GetVerts( const I_:Integer ) :Single;
begin
     if upPoins then
     begin
          MakeVerts;

          upPoins := False;
     end;

     Result := _Verts[ I_ - VertMinI ];
end;

procedure TSingleBSInterp.SetVerts( const I_:Integer; const Verts_:Single );
begin
     _Verts[ I_ - VertMinI ] := Verts_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function BSplineHEF3( const X_:Integer ) :Single;
begin
     Result := Sqrt(2) * IntPower( 2*Sqrt(2)-3, Abs( X_ ) );
end;

function BSplineHEF4( const X_:Integer ) :Single;
begin
     Result := Sqrt(3) * IntPower( Sqrt(3)-2, Abs( X_ ) );
end;

procedure TSingleBSInterp.MakeVerts;
var
   I, X :Integer;
   C :Single;
begin
     for I := VertMinI to VertMaxI do
     begin
          C := 0;

          for X := -FilterW to +FilterW do
          begin
               C := C + BSplineHEF4( X ) * Poins[ I + X ];
          end;

          SetVerts( I, C );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TSingleBSInterp.Create;
begin
     inherited;

end;

procedure TSingleBSInterp.AfterConstruction;
begin
     inherited;

     FilterW  := 4;

     CurvMinI := 0;
     CurvMaxI := 8;
end;

destructor TSingleBSInterp.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TSingleBSInterp.Curv( const X_:Single ) :Single;
var
   Xi :Integer;
   Xd :Single;
begin
     if upPoins then
     begin
          MakeVerts;

          upPoins := False;
     end;

     Xi := Floor( X_ );  Xd := X_ - Xi;

     Result := BSpline4( Verts[ Xi-1 ],
                         Verts[ Xi   ],
                         Verts[ Xi+1 ],
                         Verts[ Xi+2 ], Xd );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■