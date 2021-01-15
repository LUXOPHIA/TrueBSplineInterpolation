unit LUX.Draw.Scener;

interface //#################################################################### ■

uses System.Types, System.UITypes,
     FMX.Graphics,
     LUX, LUX.D1, LUX.D2,
     LUX.Draw.Viewer, LUX.Draw.Shaper;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TDrawPlots = class;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawPlots

     TDrawPlots = class( TDrawScener )
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawPlots

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawPlots.Create;
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

destructor TDrawPlots.Destroy;
begin

     inherited;
end;

end. //######################################################################### ■
