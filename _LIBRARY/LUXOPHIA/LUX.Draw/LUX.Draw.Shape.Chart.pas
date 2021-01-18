unit LUX.Draw.Shape.Chart;

interface //#################################################################### ■

uses System.Types, System.UITypes,
     FMX.Graphics,
     LUX, LUX.D1, LUX.D2,
     LUX.Draw.Scene,
     LUX.Draw.Shape,
     LUX.Draw.Viewer;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TDrawScal   = class;
       TDrawScaX = class;
       TDrawScaY = class;
     TDrawAxis   = class;
     TDrawGrid   = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScal

     TDrawScal = class( TDrawShape )
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
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScaY

     TDrawScaY = class( TDrawScal )
     protected
       ///// メソッド
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       destructor Destroy; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawAxis

     TDrawAxis = class( TDrawShape )
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
       _Axis   :TDrawAxis;
       _ScalX0 :TDrawScaX;
       _ScalY0 :TDrawScaY;
       _ScalX1 :TDrawScaX;
       _ScalY1 :TDrawScaY;
       _ScalX2 :TDrawScaX;
       _ScalY2 :TDrawScaY;
       ///// アクセス
       ///// メソッド
       procedure UpdateArea; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Axis   :TDrawAxis read _Axis  ;
       property ScalX0 :TDrawScaX read _ScalX0;
       property ScalY0 :TDrawScaY read _ScalY0;
       property ScalX1 :TDrawScaX read _ScalX1;
       property ScalY1 :TDrawScaY read _ScalY1;
       property ScalX2 :TDrawScaX read _ScalX2;
       property ScalY2 :TDrawScaY read _ScalY2;
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

procedure TDrawScaX.DrawMain( const Canvas_:TCanvas );
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

procedure TDrawScaY.DrawMain( const Canvas_:TCanvas );
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

     _Stroke.Color := TAlphaColorF.Create( 1/2, 1/2, 1/2 ).ToAlphaColor;
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

     _ScalX2.Area := Area;
     _ScalY2.Area := Area;
     _ScalX1.Area := Area;
     _ScalY1.Area := Area;
     _ScalX0.Area := Area;
     _ScalY0.Area := Area;
     _Axis  .Area := Area;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawGrid.Create;
begin
     inherited;

     _ScalX2 := TDrawScaX.Create( Self );
     _ScalY2 := TDrawScaY.Create( Self );
     _ScalX1 := TDrawScaX.Create( Self );
     _ScalY1 := TDrawScaY.Create( Self );
     _ScalX0 := TDrawScaX.Create( Self );
     _ScalY0 := TDrawScaY.Create( Self );
     _Axis   := TDrawAxis.Create( Self );

     with _ScalX2 do
     begin
          Interv       := 1/10;
          Stroke.Color := TAlphaColorF.Create( 15/16, 15/16, 15/16 ).ToAlphaColor;
     end;

     with _ScalY2 do
     begin
          Interv       := 1/10;
          Stroke.Color := TAlphaColorF.Create( 15/16, 15/16, 15/16 ).ToAlphaColor;
     end;

     with _ScalX1 do
     begin
          Interv       := 1/2;
          Stroke.Color := TAlphaColorF.Create( 7/8, 7/8, 7/8 ).ToAlphaColor;
     end;

     with _ScalY1 do
     begin
          Interv       := 1/2;
          Stroke.Color := TAlphaColorF.Create( 7/8, 7/8, 7/8 ).ToAlphaColor;
     end;

     with _ScalX0 do
     begin
          Interv       := 1;
          Stroke.Color := TAlphaColorF.Create( 3/4, 3/4, 3/4 ).ToAlphaColor;
     end;

     with _ScalY0 do
     begin
          Interv       := 1;
          Stroke.Color := TAlphaColorF.Create( 3/4, 3/4, 3/4 ).ToAlphaColor;
     end;
end;

destructor TDrawGrid.Destroy;
begin

     inherited;
end;

end. //######################################################################### ■
