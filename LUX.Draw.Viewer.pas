unit LUX.Draw.Viewer;

interface //#################################################################### ■

uses System.Types, System.UITypes, System.Classes,
     FMX.Graphics, FMX.Controls, FMX.Forms,
     LUX, LUX.D1, LUX.D2,
     LUX.Data.Tree;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TDrawNode     = class;
       TDrawScener = class;
     TDrawViewer   = class;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawNode

     TDrawNode = class( TTreeNode<TDrawNode,TDrawNode> )
     private
     protected
       _Opacity :Single;
       _Stroke  :TStrokeBrush;
       _Filler  :TBrush;
       ///// アクセス
       function GetViewer :TDrawViewer; virtual;
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
       property Viewer  :TDrawViewer read GetViewer                  ;
       property Opacity :Single       read GetOpacity write SetOpacity;
       property Stroke  :TStrokeBrush read   _Stroke                  ;
       property Filler  :TBrush       read   _Filler                  ;
       ///// メソッド
       procedure Draw;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScener

     TDrawScener = class( TDrawNode )
     private
     protected
       _Viewer    :TDrawViewer;
       _BackColor :TAlphaColor;
       ///// アクセス
       function GetViewer :TDrawViewer; override;
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

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawViewer

     TDrawViewer = class( TFrame )
     private
       { private 宣言 }
     protected
       _Area   :TSingleArea2D;
       _Camera :TDrawScener;
       ///// アクセス
       function GetMinX :Single;
       procedure SetMinX( const MinX_:Single );
       function GetMaxX :Single;
       procedure SetMaxX( const MaxX_:Single );
       function GetMinY :Single;
       procedure SetMinY( const MinY_:Single );
       function GetMaxY :Single;
       procedure SetMaxY( const MaxY_:Single );
       function GetCamera :TDrawScener;
       procedure SetCamera( const Draws_:TDrawScener );
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
       property Camera :TDrawScener read GetCamera write SetCamera;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawNode

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawNode.GetViewer :TDrawViewer;
begin
     Result := ( RootNode as TDrawScener ).Viewer;
end;

//------------------------------------------------------------------------------

function TDrawNode.GetOpacity :Single;
begin
     Result := _Opacity;
end;

procedure TDrawNode.SetOpacity( const Opacity_:Single );
begin
     _Opacity := Opacity_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawNode.DrawBegin;
begin
     with Viewer.Canvas do
     begin
          if Assigned( _Stroke ) then Stroke.Assign( _Stroke );
          if Assigned( _Filler ) then Fill  .Assign( _Filler );
     end;
end;

procedure TDrawNode.DrawMain;
begin

end;

procedure TDrawNode.DrawEnd;
var
   I :Integer;
begin
     for I := 0 to ChildsN-1 do Childs[ I ].Draw;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawNode.Create;
begin
     inherited;

     _Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColors.Null );
     _Filler := TBrush      .Create( TBrushKind.Solid, TAlphaColors.Null );

     _Opacity     := 1;
     _Stroke.Join := TStrokeJoin.Round;
end;

destructor TDrawNode.Destroy;
begin
     _Stroke.DisposeOf;
     _Filler.DisposeOf;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawNode.Draw;
begin
     DrawBegin;
     DrawMain;
     DrawEnd;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScener

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawScener.GetViewer :TDrawViewer;
begin
     Result := _Viewer;
end;

//------------------------------------------------------------------------------

function TDrawScener.GetBackColor :TAlphaColor;
begin
     Result := _BackColor;
end;

procedure TDrawScener.SetBackColor( const BackColor_:TAlphaColor );
begin
     _BackColor := BackColor_;  Viewer.Repaint;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawScener.DrawMain;
begin
     Viewer.Canvas.Clear( _BackColor );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawScener.Create;
begin
     inherited;

     _BackColor := TAlphaColors.White;
end;

destructor TDrawScener.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawViewer

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawViewer.GetMinX :Single;
begin
     Result := _Area.Min.X;
end;

procedure TDrawViewer.SetMinX( const MinX_:Single );
begin
     _Area.Min.X := MinX_;  Repaint;
end;

function TDrawViewer.GetMaxX :Single;
begin
     Result := _Area.Max.X;
end;

procedure TDrawViewer.SetMaxX( const MaxX_:Single );
begin
     _Area.Max.X := MaxX_;  Repaint;
end;

function TDrawViewer.GetMinY :Single;
begin
     Result := _Area.Min.Y;
end;

procedure TDrawViewer.SetMinY( const MinY_:Single );
begin
     _Area.Min.Y := MinY_;  Repaint;
end;

function TDrawViewer.GetMaxY :Single;
begin
     Result := _Area.Max.Y;
end;

procedure TDrawViewer.SetMaxY( const MaxY_:Single );
begin
     _Area.Max.Y := MaxY_;  Repaint;
end;

//------------------------------------------------------------------------------

function TDrawViewer.GetCamera :TDrawScener;
begin
     Result := _Camera;
end;

procedure TDrawViewer.SetCamera( const Draws_:TDrawScener );
begin
     _Camera := Draws_;

     _Camera._Viewer := Self;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawViewer.Paint;
begin
     inherited;

     if Assigned( _Camera ) then _Camera.Draw;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawViewer.Create( Owner_:TComponent );
begin
     inherited;

     AutoCapture := True;

     _Camera := nil;

     MinX  := -2.0;
     MaxX  := +2.0;
     MinY  := -1.5;
     MaxY  := +1.5;
end;

destructor TDrawViewer.Destroy;
begin
     if Assigned( _Camera ) then _Camera.DisposeOf;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TDrawViewer.PosToScrX( const Pos_:Single ) :Single;
begin
     Result := ( Pos_ - _Area.Min.X ) / ( _Area.Max.X - _Area.Min.X ) * Width ;
end;

function TDrawViewer.PosToScrY( const Pos_:Single ) :Single;
begin
     Result := ( Pos_ - _Area.Max.Y ) / ( _Area.Min.Y - _Area.Max.Y ) * Height;
end;

function TDrawViewer.PosToScr( const Pos_:TSingle2D ) :TPointF;
begin
     Result.X := PosToScrX( Pos_.X );
     Result.Y := PosToScrY( Pos_.Y );
end;

function TDrawViewer.ScrToPosX( const Scr_:Single ) :Single;
begin
     Result := Scr_ / Width  * ( _Area.Max.X - _Area.Min.X ) + _Area.Min.X;
end;

function TDrawViewer.ScrToPosY( const Scr_:Single ) :Single;
begin
     Result := Scr_ / Height * ( _Area.Min.Y - _Area.Max.Y ) + _Area.Max.Y;
end;

function TDrawViewer.ScrToPos( const Scr_:TPointF ) :TSingle2D;
begin
     Result.X := ScrToPosX( Scr_.X );
     Result.Y := ScrToPosY( Scr_.Y );
end;

end. //######################################################################### ■
