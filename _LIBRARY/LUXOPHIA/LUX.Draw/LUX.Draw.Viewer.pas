unit LUX.Draw.Viewer;

interface //#################################################################### ■

uses System.Types, System.UITypes, System.Classes,
     FMX.Graphics, FMX.Controls, FMX.Forms,
     LUX, LUX.D1, LUX.D2,
     LUX.Data.Tree;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TChartNode     = class;
       TChartScener = class;
     TChartViewer   = class;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TChartNode

     TChartNode = class( TTreeNode<TChartNode,TChartNode> )
     private
     protected
       _Opacity :Single;
       _Stroke  :TStrokeBrush;
       _Filler  :TBrush;
       ///// アクセス
       function GetViewer :TChartViewer; virtual;
       function GetOpacity :Single;
       procedure SetOpacity( const Opacity_:Single );
       ///// メソッド
       procedure DrawBegin; virtual;
       procedure DrawMain; virtual;
       procedure DrawEnd; virtual;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Viewer  :TChartViewer read GetViewer                  ;
       property Opacity :Single       read GetOpacity write SetOpacity;
       property Stroke  :TStrokeBrush read   _Stroke                  ;
       property Filler  :TBrush       read   _Filler                  ;
       ///// メソッド
       procedure Draw;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TChartScener

     TChartScener = class( TChartNode )
     private
     protected
       _Viewer    :TChartViewer;
       _BackColor :TAlphaColor;
       ///// アクセス
       function GetViewer :TChartViewer; override;
       function GetBackColor :TAlphaColor;
       procedure SetBackColor( const BackColor_:TAlphaColor );
       ///// メソッド
       procedure DrawMain; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property BackColor :TAlphaColor read GetBackColor write SetBackColor;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TChartViewer

     TChartViewer = class( TFrame )
     private
       { private 宣言 }
     protected
       _Area   :TSingleArea2D;
       _Camera :TChartScener;
       ///// アクセス
       function GetMinX :Single;
       procedure SetMinX( const MinX_:Single );
       function GetMaxX :Single;
       procedure SetMaxX( const MaxX_:Single );
       function GetMinY :Single;
       procedure SetMinY( const MinY_:Single );
       function GetMaxY :Single;
       procedure SetMaxY( const MaxY_:Single );
       function GetCamera :TChartScener;
       procedure SetCamera( const Charts_:TChartScener );
       ///// メソッド
       procedure Paint; override;
     public
       { public 宣言 }
       constructor Create( Owner_:TComponent ); override;
       destructor Destroy; override;
       ///// プロパティ
       property MinX   :Single       read GetMinX   write SetMinX  ;
       property MaxX   :Single       read GetMaxX   write SetMaxX  ;
       property MinY   :Single       read GetMinY   write SetMinY  ;
       property MaxY   :Single       read GetMaxY   write SetMaxY  ;
       property Camera :TChartScener read GetCamera write SetCamera;
       ///// メソッド
       function PosToScrX( const Pos_:Single ) :Single;
       function PosToScrY( const Pos_:Single ) :Single;
       function PosToScr( const Pos_:TSingle2D ) :TPointF;
       function ScrToPosX( const Scr_:Single ) :Single;
       function ScrToPosY( const Scr_:Single ) :Single;
       function ScrToPos( const Scr_:TPointF ) :TSingle2D;
     end;

implementation //############################################################### ■

{$R *.fmx}

uses System.Math;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TChartNode

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TChartNode.GetViewer :TChartViewer;
begin
     Result := ( RootNode as TChartScener ).Viewer;
end;

//------------------------------------------------------------------------------

function TChartNode.GetOpacity :Single;
begin
     Result := _Opacity;
end;

procedure TChartNode.SetOpacity( const Opacity_:Single );
begin
     _Opacity := Opacity_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TChartNode.DrawBegin;
begin
     with Viewer.Canvas do
     begin
          if Assigned( _Stroke ) then Stroke.Assign( _Stroke );
          if Assigned( _Filler ) then Fill  .Assign( _Filler );
     end;
end;

procedure TChartNode.DrawMain;
begin

end;

procedure TChartNode.DrawEnd;
var
   I :Integer;
begin
     for I := 0 to ChildsN-1 do Childs[ I ].Draw;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TChartNode.Create;
begin
     inherited;

     _Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColors.Null );
     _Filler := TBrush      .Create( TBrushKind.Solid, TAlphaColors.Null );

     _Opacity     := 1;
     _Stroke.Join := TStrokeJoin.Round;
end;

destructor TChartNode.Destroy;
begin
     _Stroke.DisposeOf;
     _Filler.DisposeOf;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TChartNode.Draw;
begin
     DrawBegin;
     DrawMain;
     DrawEnd;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TChartScener

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TChartScener.GetViewer :TChartViewer;
begin
     Result := _Viewer;
end;

//------------------------------------------------------------------------------

function TChartScener.GetBackColor :TAlphaColor;
begin
     Result := _BackColor;
end;

procedure TChartScener.SetBackColor( const BackColor_:TAlphaColor );
begin
     _BackColor := BackColor_;  Viewer.Repaint;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TChartScener.DrawMain;
begin
     Viewer.Canvas.Clear( _BackColor );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TChartScener.Create;
begin
     inherited;

     _BackColor := TAlphaColors.White;
end;

destructor TChartScener.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TChartViewer

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TChartViewer.GetMinX :Single;
begin
     Result := _Area.Min.X;
end;

procedure TChartViewer.SetMinX( const MinX_:Single );
begin
     _Area.Min.X := MinX_;  Repaint;
end;

function TChartViewer.GetMaxX :Single;
begin
     Result := _Area.Max.X;
end;

procedure TChartViewer.SetMaxX( const MaxX_:Single );
begin
     _Area.Max.X := MaxX_;  Repaint;
end;

function TChartViewer.GetMinY :Single;
begin
     Result := _Area.Min.Y;
end;

procedure TChartViewer.SetMinY( const MinY_:Single );
begin
     _Area.Min.Y := MinY_;  Repaint;
end;

function TChartViewer.GetMaxY :Single;
begin
     Result := _Area.Max.Y;
end;

procedure TChartViewer.SetMaxY( const MaxY_:Single );
begin
     _Area.Max.Y := MaxY_;  Repaint;
end;

//------------------------------------------------------------------------------

function TChartViewer.GetCamera :TChartScener;
begin
     Result := _Camera;
end;

procedure TChartViewer.SetCamera( const Charts_:TChartScener );
begin
     _Camera := Charts_;

     _Camera._Viewer := Self;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TChartViewer.Paint;
begin
     inherited;

     if Assigned( _Camera ) then _Camera.Draw;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TChartViewer.Create( Owner_:TComponent );
begin
     inherited;

     AutoCapture := True;

     _Camera := nil;

     MinX  := -2.0;
     MaxX  := +2.0;
     MinY  := -1.5;
     MaxY  := +1.5;
end;

destructor TChartViewer.Destroy;
begin
     if Assigned( _Camera ) then _Camera.DisposeOf;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TChartViewer.PosToScrX( const Pos_:Single ) :Single;
begin
     Result := ( Pos_ - _Area.Min.X ) / ( _Area.Max.X - _Area.Min.X ) * Width ;
end;

function TChartViewer.PosToScrY( const Pos_:Single ) :Single;
begin
     Result := ( Pos_ - _Area.Max.Y ) / ( _Area.Min.Y - _Area.Max.Y ) * Height;
end;

function TChartViewer.PosToScr( const Pos_:TSingle2D ) :TPointF;
begin
     Result.X := PosToScrX( Pos_.X );
     Result.Y := PosToScrY( Pos_.Y );
end;

function TChartViewer.ScrToPosX( const Scr_:Single ) :Single;
begin
     Result := Scr_ / Width  * ( _Area.Max.X - _Area.Min.X ) + _Area.Min.X;
end;

function TChartViewer.ScrToPosY( const Scr_:Single ) :Single;
begin
     Result := Scr_ / Height * ( _Area.Min.Y - _Area.Max.Y ) + _Area.Max.Y;
end;

function TChartViewer.ScrToPos( const Scr_:TPointF ) :TSingle2D;
begin
     Result.X := ScrToPosX( Scr_.X );
     Result.Y := ScrToPosY( Scr_.Y );
end;

end. //######################################################################### ■
