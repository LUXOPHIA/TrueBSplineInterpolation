unit CurveChartFrame;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  LUX, LUX.D1, LUX.D2,
  LUX.Draw.Scene, LUX.Draw.Shape, LUX.Draw.Shape.Chart, LUX.Draw.Viewer;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TCurveChart

     TCurveChart = class( TDrawViewer )
     private
       { private 宣言 }
       _Scene :TDrawScene;
       _Grids :TDrawGrids;
       _Poins :TDrawCopys1D;
       _Verts :TDrawCopys1D;
       _Curv  :TDrawCurv1D;
       ///// アクセス
       function GetMinX :Single;
       procedure SetMinX( const MinX_:Single );
       function GetMaxX :Single;
       procedure SetMaxX( const MaxX_:Single );
       ///// メソッド
       procedure InitChart;
     public
       { public 宣言 }
       constructor Create( Owner_:TComponent ); override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property MinX  :Single       read GetMinX  write SetMinX;
       property MaxX  :Single       read GetMaxX  write SetMaxX;
       property Poins :TDrawCopys1D read   _Poins              ;
       property Verts :TDrawCopys1D read   _Verts              ;
       property Curv  :TDrawCurv1D  read   _Curv               ;
     end;

implementation //############################################################### ■

{$R *.fmx}

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TCurveChart

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TCurveChart.GetMinX :Single;
begin
     Result := _Curv.MinX;
end;

procedure TCurveChart.SetMinX( const MinX_:Single );
begin
     _Curv.MinX := MinX_;  InitChart;
end;

function TCurveChart.GetMaxX :Single;
begin
     Result := _Curv.MaxX;
end;

procedure TCurveChart.SetMaxX( const MaxX_:Single );
begin
     _Curv.MaxX := MaxX_;  InitChart;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TCurveChart.InitChart;
begin
     Camera.Area := TSingleArea2D.Create( MinX-2, -3, MaxX+2, +3 );

     with _Grids do
     begin
          Axis .Area := TSingleArea2D.Create( MinX-2, -3, MaxX+2, +3 );
          Grid1.Area := TSingleArea2D.Create( MinX-1, -2, MaxX+1, +2 );
          Grid2.Area := TSingleArea2D.Create( MinX  , -2, MaxX  , +2 );
          Grid3.Area := TSingleArea2D.Create( MinX  , -2, MaxX  , +2 );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TCurveChart.Create( Owner_:TComponent );
begin
     inherited;

     _Scene := TDrawScene.Create;

     Camera := TDrawCamera.Create( _Scene );

     _Grids := TDrawGrids  .Create( _Scene );
     _Verts := TDrawCopys1D.Create( _Scene );
     _Curv  := TDrawCurv1D .Create( _Scene );
     _Poins := TDrawCopys1D.Create( _Scene );

     with TDrawCirc.Create( _Poins ) do
     begin
          Radius           := 0.1;
          Stroke           := TStrokeBrush.Create( TBrushKind.Solid, 1 );
          Stroke.Color     := TAlphaColorF.Create( 255/255, 133/255, 133/255 ).ToAlphaColor;
          Stroke.Thickness := 0.05;
          Filler           := TBrush.Create( TBrushKind.None, 1 );
     end;

     with TDrawCirc.Create( _Verts ) do
     begin
          Radius           := 0.1;
          Stroke           := TStrokeBrush.Create( TBrushKind.Solid, 1 );
          Stroke.Color     := TAlphaColorF.Create( 1, 1, 1 ).ToAlphaColor;
          Stroke.Thickness := 0.05;
          Filler           := TBrush.Create( TBrushKind.Solid, 1 );
          Filler.Color     := TAlphaColorF.Create( 115/255, 222/255, 115/255 ).ToAlphaColor;
     end;

     with _Curv do
     begin
          Stroke           := TStrokeBrush.Create( TBrushKind.Solid, 1 );
          Stroke.Color     := TAlphaColorF.Create( 133/255, 133/255, 255/255 ).ToAlphaColor;
          Stroke.Cap       := TStrokeCap.Round;
          Stroke.Thickness := 0.1;
     end;
end;

procedure TCurveChart.AfterConstruction;
begin
     inherited;

     MinX := 0;
     MaxX := 8;
end;

destructor TCurveChart.Destroy;
begin
     _Scene.Free;

     inherited;
end;

end. //######################################################################### ■
