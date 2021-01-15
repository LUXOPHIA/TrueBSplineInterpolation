unit LUX.Draw.Shaper;

interface //#################################################################### ■

uses System.Types, System.UITypes,
     FMX.Controls, FMX.Graphics,
     LUX, LUX.D1, LUX.D2,
     LUX.FMX.Graphics,
     LUX.Draw.Viewer;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TDrawPoin = class;
     TDrawCurv = class;
     TDrawScal = class;
       TDrawScaX = class;
       TDrawScaY = class;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawPoin

     TDrawPoin = class( TDrawNode )
     private
     protected
       _Pos    :TSingle2D;
       _Radius :Single;
       _Border :Single;
       ///// アクセス
       function GetPos :TSingle2D;
       procedure SetPos( const Pos_:TSingle2D );
       function GetRadius :Single;
       procedure SetRadius( const Radius_:Single );
       function GetBorder :Single;
       procedure SetBorder( const Border_:Single );
       ///// メソッド
       procedure DrawMain; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Pos    :TSingle2D read GetPos    write SetPos   ;
       property Radius :Single    read GetRadius write SetRadius;
       property Border :Single    read GetBorder write SetBorder;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCurv

     TDrawCurv = class( TDrawNode )
     private
       _Path :TPathData;
     protected
       _Poins   :TArray<TSingle2D>;
       _PoinsN  :Integer;
       ///// アクセス
       function GetPoins( const I_:Integer ) :TSingle2D;
       procedure SetPoins( const I_:Integer; const Value_:TSingle2D );
       function GetPoinsN :Integer;
       procedure SetPoinsN( const ValuesN_:Integer );
       ///// メソッド
       procedure DrawMain; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Poins[ const I_:Integer ] :TSingle2D read GetPoins  write SetPoins ; default;
       property PoinsN                    :Integer   read GetPoinsN write SetPoinsN;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScal

     TDrawScal = class( TDrawNode )
     protected
       _Interv :Single;
       ///// アクセス
       function GetInterv :Single;
       procedure SetInterv( const Interv_:Single );
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Interv :Single read GetInterv write SetInterv;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScaX

     TDrawScaX = class( TDrawScal )
     protected
       ///// メソッド
       procedure DrawMain; override;
     public
       constructor Create; override;
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScaY

     TDrawScaY = class( TDrawScal )
     protected
       ///// メソッド
       procedure DrawMain; override;
     public
       constructor Create; override;
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxis

     TDrawAxis = class( TDrawNode )
     protected
       ///// メソッド
       procedure DrawMain; override;
     public
       constructor Create; override;
       destructor Destroy; override;
     end;

implementation //############################################################### ■

uses System.Math;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawPoin

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawPoin.GetPos :TSingle2D;
begin
     Result := _Pos;
end;

procedure TDrawPoin.SetPos( const Pos_:TSingle2D );
begin
     _Pos := Pos_;
end;

//------------------------------------------------------------------------------

function TDrawPoin.GetRadius :Single;
begin
     Result := _Radius;
end;

procedure TDrawPoin.SetRadius( const Radius_:Single );
begin
     _Radius := Radius_;
end;

function TDrawPoin.GetBorder :Single;
begin
     Result := _Border;
end;

procedure TDrawPoin.SetBorder( const Border_:Single );
begin
     _Border := Border_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawPoin.DrawMain;
begin
     inherited;

     with Viewer.Canvas do
     begin
          DrawCircle( _Pos, _Radius, _Opacity );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawPoin.Create;
begin
     inherited;

     _Pos    := TSingle2D.Create( 0, 0 );
     _Radius := 5;
     _Border := 0;
end;

destructor TDrawPoin.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCurv

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawCurv.GetPoins( const I_:Integer ) :TSingle2D;
begin
     Result := _Poins[ I_ ];
end;

procedure TDrawCurv.SetPoins( const I_:Integer; const Value_:TSingle2D );
begin
     _Poins[ I_ ] := Value_;
end;

function TDrawCurv.GetPoinsN :Integer;
begin
     Result := _PoinsN;
end;

procedure TDrawCurv.SetPoinsN( const ValuesN_:Integer );
begin
     _PoinsN := ValuesN_;

     SetLength( _Poins, _PoinsN );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawCurv.Create;
begin
     inherited;

     _Path := TPathData.Create;

     PoinsN := 100;
end;

destructor TDrawCurv.Destroy;
begin
     _Path.DisposeOf;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawCurv.DrawMain;
var
   I :Integer;
begin
     inherited;

     with Viewer do
     begin
          with _Path do
          begin
               Clear;

               MoveTo( PosToScr( _Poins[ 0 ] ) );

               for I := 1 to _PoinsN-1 do LineTo( PosToScr( _Poins[ I ] ) );
          end;

          Canvas.DrawPath( _Path, _Opacity );
     end;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawGrid

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawScal.GetInterv :Single;
begin
     Result := _Interv;
end;

procedure TDrawScal.SetInterv( const Interv_:Single );
begin
     _Interv := Interv_;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawScal.Create;
begin
     inherited;

     _Interv := 0.1;
end;

destructor TDrawScal.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScaX

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawScaX.Create;
begin
     inherited;

end;

destructor TDrawScaX.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawScaX.DrawMain;
var
   I0, I1, I :Integer;
   X :Single;
   P0, P1 :TSingle2D;
begin
     inherited;

     with Viewer do
     begin
          I0 := Ceil ( MinX / _Interv );
          I1 := Floor( MaxX / _Interv );

          P0.Y := MinY;
          P1.Y := MaxY;
          for I := I0 to I1 do
          begin
               X := I * _Interv;

               P0.X := X;
               P1.X := X;

               Canvas.DrawLine( PosToScr( P0 ), PosToScr( P1 ), _Opacity );
          end;
     end;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScaY

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawScaY.Create;
begin
     inherited;

end;

destructor TDrawScaY.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawScaY.DrawMain;
var
   I0, I1, I :Integer;
   Y :Single;
   P0, P1 :TSingle2D;
begin
     inherited;

     with Viewer do
     begin
          I0 := Ceil ( MinY / _Interv );
          I1 := Floor( MaxY / _Interv );

          P0.X := MinX;
          P1.X := MaxX;
          for I := I0 to I1 do
          begin
               Y := I * _Interv;

               P0.Y := Y;
               P1.Y := Y;

               Canvas.DrawLine( PosToScr( P0 ), PosToScr( P1 ), _Opacity );
          end;
     end;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxis

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawAxis.DrawMain;
var
   P0, P1 :TSingle2D;
begin
     inherited;

     with Viewer do
     begin
          with Canvas do
          begin
               P0.X := MinX;  P0.Y := 0;
               P1.X := MaxX;  P1.Y := 0;

               DrawLine( PosToScr( P0 ), PosToScr( P1 ), _Opacity );

               P0.X := 0;  P0.Y := MinY;
               P1.X := 0;  P1.Y := MaxY;

               DrawLine( PosToScr( P0 ), PosToScr( P1 ), _Opacity );
          end;
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawAxis.Create;
begin
     inherited;

     _Stroke.Color := TAlphaColorF.Create( 1/2, 1/2, 1/2 ).ToAlphaColor;
end;

destructor TDrawAxis.Destroy;
begin

     inherited;
end;

end. //######################################################################### ■
