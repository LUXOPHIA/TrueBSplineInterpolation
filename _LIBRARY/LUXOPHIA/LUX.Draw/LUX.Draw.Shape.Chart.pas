unit LUX.Draw.Shape.Chart;

interface //#################################################################### ■

uses System.Types, System.UITypes,
     FMX.Graphics,
     LUX, LUX.D1, LUX.D2,
     LUX.Draw.Scene,
     LUX.Draw.Shape,
     LUX.Draw.Viewer;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TDrawScal     = class;
       TDrawScalX  = class;
       TDrawScalY  = class;
       TDrawScalXY = class;
     TDrawAxis     = class;
     TDrawGrid     = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScal

     TDrawScal = class( TDrawShape )
     private
     protected
       _Interv :Single;
       ///// アクセス
       function GetInterv :Single; virtual;
       procedure SetInterv( const Interv_:Single ); virtual;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Interv :Single read GetInterv write SetInterv;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScalX

     TDrawScalX = class( TDrawScal )
     private
     protected
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScalY

     TDrawScalY = class( TDrawScal )
     private
     protected
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScalXY

     TDrawScalXY = class( TDrawScal )
     private
     protected
       _ScalX :TDrawScalX;
       _ScalY :TDrawScalY;
       ///// アクセス
       function GetInterv :Single; override;
       procedure SetInterv( const Interv_:Single ); override;
       ///// メソッド
       procedure UpdateArea; override;
     public
       constructor Create; override;
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxis

     TDrawAxis = class( TDrawShape )
     private
     protected
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawGrid

     TDrawGrid = class( TDrawShape )
     private
     protected
       _Scal0 :TDrawScalXY;
       _Scal1 :TDrawScalXY;
       _Scal2 :TDrawScalXY;
       ///// アクセス
       ///// メソッド
       procedure UpdateArea; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Scal0 :TDrawScalXY read _Scal0;
       property Scal1 :TDrawScalXY read _Scal1;
       property Scal2 :TDrawScalXY read _Scal2;
     end;

implementation //############################################################### ■

uses System.Math;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScal

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

     MinX := -10;  MaxX := +10;
     MinY := -10;  MaxY := +10;

     _Interv := 0.1;
end;

destructor TDrawScal.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScalX

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawScalX.DrawMain( const Canvas_:TCanvas );
var
   I0, I1, I :Integer;
   X :Single;
   P0, P1 :TSingle2D;
begin
     inherited;

     I0 := Ceil ( MinX / _Interv );
     I1 := Floor( MaxX / _Interv );

     P0.Y := MinY;
     P1.Y := MaxY;
     for I := I0 to I1 do
     begin
          X := I * _Interv;

          P0.X := X;
          P1.X := X;

          Canvas_.DrawLine( P0, P1, _Opacity );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawScalX.Create;
begin
     inherited;

end;

destructor TDrawScalX.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScalY

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawScalY.DrawMain( const Canvas_:TCanvas );
var
   I0, I1, I :Integer;
   Y :Single;
   P0, P1 :TSingle2D;
begin
     inherited;

     I0 := Ceil ( MinY / _Interv );
     I1 := Floor( MaxY / _Interv );

     P0.X := MinX;
     P1.X := MaxX;
     for I := I0 to I1 do
     begin
          Y := I * _Interv;

          P0.Y := Y;
          P1.Y := Y;

          Canvas_.DrawLine( P0, P1, _Opacity );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawScalY.Create;
begin
     inherited;

end;

destructor TDrawScalY.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScalXY

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawScalXY.GetInterv :Single;
begin
     Result := Min( _ScalX.Interv, _ScalY.Interv );
end;

procedure TDrawScalXY.SetInterv( const Interv_:Single );
begin
     _ScalX.Interv := Interv_;
     _ScalY.Interv := Interv_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawScalXY.UpdateArea;
begin
     inherited;

     _ScalX.Area := Area;
     _ScalY.Area := Area;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawScalXY.Create;
begin
     inherited;

     _ScalX := TDrawScalX.Create( Self );
     _ScalY := TDrawScalY.Create( Self );
end;

destructor TDrawScalXY.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxis

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawAxis.DrawMain( const Canvas_:TCanvas );
var
   P0, P1 :TSingle2D;
begin
     inherited;

     with Canvas_ do
     begin
          P0.X := MinX;  P0.Y := 0;
          P1.X := MaxX;  P1.Y := 0;

          DrawLine( P0, P1, _Opacity );

          P0.X := 0;  P0.Y := MinY;
          P1.X := 0;  P1.Y := MaxY;

          DrawLine( P0, P1, _Opacity );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawAxis.Create;
begin
     inherited;

     _Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColorF.Create( 1/2, 1/2, 1/2 ).ToAlphaColor );
end;

destructor TDrawAxis.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawGrid

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawGrid.UpdateArea;
begin
     inherited;

     _Scal2.Area := Area;
     _Scal1.Area := Area;
     _Scal0.Area := Area;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawGrid.Create;
begin
     inherited;

     _Scal2 := TDrawScalXY.Create( Self );
     _Scal1 := TDrawScalXY.Create( Self );
     _Scal0 := TDrawScalXY.Create( Self );

     with _Scal2 do
     begin
          Interv := 1/10;
          Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColorF.Create( 15/16, 15/16, 15/16 ).ToAlphaColor );
          Stroke.Thickness := 0.02;
     end;

     with _Scal1 do
     begin
          Interv := 1/2;
          Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColorF.Create( 7/8, 7/8, 7/8 ).ToAlphaColor );
          Stroke.Thickness := 0.02;
     end;

     with _Scal0 do
     begin
          Interv := 1;
          Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColorF.Create( 3/4, 3/4, 3/4 ).ToAlphaColor );
          Stroke.Thickness := 0.02;
     end;
end;

destructor TDrawGrid.Destroy;
begin

     inherited;
end;

end. //######################################################################### ■
