unit LUX.Draw.Scener;

interface //#################################################################### ■

uses System.Types, System.UITypes,
     FMX.Graphics,
     LUX, LUX.D1, LUX.D2,
     LUX.Draw.Viewer, LUX.Draw.Shaper;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TChartPlots = class;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TChartPlots

     TChartPlots = class( TChartScener )
     private
     protected
       _Axis   :TChartAxis;
       _ScalX0 :TChartScaX;
       _ScalY0 :TChartScaY;
       _ScalX1 :TChartScaX;
       _ScalY1 :TChartScaY;
       _ScalX2 :TChartScaX;
       _ScalY2 :TChartScaY;
       ///// アクセス
       ///// メソッド
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Axis   :TChartAxis read _Axis  ;
       property ScalX0 :TChartScaX read _ScalX0;
       property ScalY0 :TChartScaY read _ScalY0;
       property ScalX1 :TChartScaX read _ScalX1;
       property ScalY1 :TChartScaY read _ScalY1;
       property ScalX2 :TChartScaX read _ScalX2;
       property ScalY2 :TChartScaY read _ScalY2;
     end;

implementation //############################################################### ■

uses System.Math;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TChartPlots

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TChartPlots.Create;
begin
     inherited;

     _ScalX2 := TChartScaX.Create( Self );
     _ScalY2 := TChartScaY.Create( Self );
     _ScalX1 := TChartScaX.Create( Self );
     _ScalY1 := TChartScaY.Create( Self );
     _ScalX0 := TChartScaX.Create( Self );
     _ScalY0 := TChartScaY.Create( Self );
     _Axis   := TChartAxis.Create( Self );

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

destructor TChartPlots.Destroy;
begin

     inherited;
end;

end. //######################################################################### ■
