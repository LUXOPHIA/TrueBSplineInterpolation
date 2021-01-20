unit LUX.Draw.Shape.Chart;

interface //#################################################################### ■

uses System.Types, System.UITypes,
     FMX.Graphics,
     LUX, LUX.D1, LUX.D2,
     LUX.Draw.Scene,
     LUX.Draw.Shape,
     LUX.Draw.Viewer;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TDrawScal    = class;
       TDrawScalX = class;
       TDrawScalY = class;
       TDrawGrid  = class;
     TDrawAxisX   = class;
     TDrawAxisY   = class;
     TDrawAxis    = class;
     TDrawGrids   = class;

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

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawGrid

     TDrawGrid = class( TDrawScal )
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
       ///// プロパティ
       property ScalX :TDrawScalX read _ScalX;
       property ScalY :TDrawScalY read _ScalY;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxisX

     TDrawAxisX = class( TDrawShape )
     private
     protected
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxisY

     TDrawAxisY = class( TDrawShape )
     private
     protected
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxis

     TDrawAxis = class( TDrawShape )
     private
     protected
       _AxisX :TDrawAxisX;
       _AxisY :TDrawAxisY;
       ///// メソッド
       procedure UpdateArea; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property AxisX :TDrawAxisX read _AxisX;
       property AxisY :TDrawAxisY read _AxisY;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawGrids

     TDrawGrids = class( TDrawShape )
     private
     protected
       _Axis  :TDrawAxis;
       _Grid1 :TDrawGrid;
       _Grid2 :TDrawGrid;
       _Grid3 :TDrawGrid;
       ///// アクセス
       ///// メソッド
       procedure UpdateArea; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Axis  :TDrawAxis read _Axis ;
       property Grid1 :TDrawGrid read _Grid1;
       property Grid2 :TDrawGrid read _Grid2;
       property Grid3 :TDrawGrid read _Grid3;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawGrid

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawGrid.GetInterv :Single;
begin
     Result := Min( _ScalX.Interv, _ScalY.Interv );
end;

procedure TDrawGrid.SetInterv( const Interv_:Single );
begin
     _ScalX.Interv := Interv_;
     _ScalY.Interv := Interv_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawGrid.UpdateArea;
begin
     inherited;

     _ScalX.Area := Area;
     _ScalY.Area := Area;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawGrid.Create;
begin
     inherited;

     _ScalX := TDrawScalX.Create( Self );
     _ScalY := TDrawScalY.Create( Self );
end;

destructor TDrawGrid.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxisX

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawAxisX.DrawMain( const Canvas_:TCanvas );
var
   P0, P1 :TSingle2D;
begin
     inherited;

     P0.X := 0;  P0.Y := MinY;
     P1.X := 0;  P1.Y := MaxY;

     Canvas_.DrawLine( P0, P1, _Opacity );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawAxisX.Create;
begin
     inherited;

end;

destructor TDrawAxisX.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxisY

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawAxisY.DrawMain( const Canvas_:TCanvas );
var
   P0, P1 :TSingle2D;
begin
     inherited;

     P0.X := MinX;  P0.Y := 0;
     P1.X := MaxX;  P1.Y := 0;

     Canvas_.DrawLine( P0, P1, _Opacity );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawAxisY.Create;
begin
     inherited;

end;

destructor TDrawAxisY.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxis

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawAxis.UpdateArea;
begin
     inherited;

     _AxisX.Area := Area;
     _AxisY.Area := Area;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawAxis.Create;
begin
     inherited;

     _AxisX := TDrawAxisX.Create( Self );
     _AxisY := TDrawAxisY.Create( Self );
end;

destructor TDrawAxis.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawGrids

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawGrids.UpdateArea;
begin
     inherited;

     _Grid3.Area := Area;
     _Grid2.Area := Area;
     _Grid1.Area := Area;
     _Axis .Area := Area;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawGrids.Create;
begin
     inherited;

     _Grid3 := TDrawGrid.Create( Self );
     _Grid2 := TDrawGrid.Create( Self );
     _Grid1 := TDrawGrid.Create( Self );
     _Axis  := TDrawAxis.Create( Self );

     with _Grid3 do
     begin
          Interv := 1/10;
          Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColorF.Create( 15/16, 15/16, 15/16 ).ToAlphaColor );
          Stroke.Thickness := 0.02;
     end;

     with _Grid2 do
     begin
          Interv := 1/2;
          Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColorF.Create( 7/8, 7/8, 7/8 ).ToAlphaColor );
          Stroke.Thickness := 0.02;
     end;

     with _Grid1 do
     begin
          Interv := 1;
          Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColorF.Create( 3/4, 3/4, 3/4 ).ToAlphaColor );
          Stroke.Thickness := 0.02;
     end;

     with _Axis do
     begin
          Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColorF.Create( 1/2, 1/2, 1/2 ).ToAlphaColor );
          Stroke.Thickness := 0.02;
     end;
end;

destructor TDrawGrids.Destroy;
begin

     inherited;
end;

end. //######################################################################### ■
