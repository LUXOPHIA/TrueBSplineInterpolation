unit LUX.Draw.Scene;

interface //#################################################################### ■

uses System.Types, System.UITypes, System.Math.Vectors,
     FMX.Graphics,
     LUX, LUX.D1, LUX.D2, LUX.Data.Tree;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TDrawNode     = class;
       TDrawScene  = class;
       TDrawCamera = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawNode

     TDrawNode = class( TTreeNode<TDrawNode,TDrawNode> )
     private
       _State :TCanvasSaveState;
     protected
       _Area    :TSingleArea2D;
       _Opacity :Single;
       _Stroke  :TStrokeBrush;
       _Filler  :TBrush;
       ///// アクセス
       function GetArea :TSingleArea2D;
       procedure SetArea( const Area_:TSingleArea2D ); virtual;
       function GetMatrix :TMatrix; virtual; abstract;
       procedure SetMatrix( const Matrix_:TMatrix ); virtual; abstract;
       function GetAbsoMatrix :TMatrix; virtual;
       procedure SetAbsoMatrix( const GlobalMatrix_:TMatrix ); virtual;
       function GetPosition :TPointF;
       procedure SetPosition( const Position_:TPointF );
       function GetOpacity :Single;
       procedure SetOpacity( const Opacity_:Single );
       function GetStroke :TStrokeBrush; virtual;
       procedure SetStroke( const Stroke_:TStrokeBrush );
       function GetFiller :TBrush; virtual;
       procedure SetFiller( const Filler_:TBrush );
       ///// メソッド
       procedure DrawBegin( const Canvas_:TCanvas ); virtual;
       procedure DrawMain( const Canvas_:TCanvas ); virtual;
       procedure DrawEnd( const Canvas_:TCanvas ); virtual;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Area       :TSingleArea2D read GetArea       write SetArea      ;
       property Matrix     :TMatrix       read GetMatrix     write SetMatrix    ;
       property Position   :TPointF       read GetPosition   write SetPosition  ;
       property AbsoMatrix :TMatrix       read GetAbsoMatrix write SetAbsoMatrix;
       property Opacity    :Single        read GetOpacity    write SetOpacity   ;
       property Stroke     :TStrokeBrush  read GetStroke     write SetStroke    ;
       property Filler     :TBrush        read GetFiller     write SetFiller    ;
       ///// メソッド
       procedure Draw( const Canvas_:TCanvas );
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawRoot

     TDrawRoot = class( TDrawNode )
     private
     protected
       ///// アクセス
       function GetMatrix :TMatrix; override;
       procedure SetMatrix( const Matrix_:TMatrix ); override;
       function GetAbsoMatrix :TMatrix; override;
       procedure SetAbsoMatrix( const GlobalMatrix_:TMatrix ); override;
       ///// メソッド
       procedure DrawBegin( const Canvas_:TCanvas ); override;
       procedure DrawMain( const Canvas_:TCanvas ); override;
       procedure DrawEnd( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property Matrix     :TMatrix read GetMatrix    ;
       property Position   :TPointF read GetPosition  ;
       property AbsoMatrix :TMatrix read GetAbsoMatrix;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScene

     TDrawScene = class( TDrawRoot )
     private
     protected
       _BackColor :TAlphaColor;
       ///// アクセス
       function GetStroke :TStrokeBrush; override;
       function GetFiller :TBrush; override;
       function GetBackColor :TAlphaColor;
       procedure SetBackColor( const BackColor_:TAlphaColor );
       ///// メソッド
       procedure DrawBegin( const Canvas_:TCanvas ); override;
       procedure DrawMain( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       property BackColor :TAlphaColor read GetBackColor write SetBackColor;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawShape

     TDrawShape = class( TDrawNode )
     private
     protected
       _Matrix :TMatrix;
       ///// アクセス
       function GetMatrix :TMatrix; override;
       procedure SetMatrix( const Matrix_:TMatrix ); override;
       ///// メソッド
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCamera

     TDrawCamera = class( TDrawShape )
     private
     protected
       ///// アクセス
       ///// メソッド
     public
       constructor Create; override;
       procedure AfterConstruction; override;
       destructor Destroy; override;
       ///// プロパティ
       ///// メソッド
       procedure Render( const Canvas_:TCanvas );
     end;

implementation //############################################################### ■

uses System.Math;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawNode

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawNode.GetArea :TSingleArea2D;
begin
     Result := _Area;
end;

procedure TDrawNode.SetArea( const Area_:TSingleArea2D );
begin
     _Area := Area_;
end;

//------------------------------------------------------------------------------

function TDrawNode.GetAbsoMatrix :TMatrix;
begin
     Result := Parent.AbsoMatrix * Matrix;
end;

procedure TDrawNode.SetAbsoMatrix( const GlobalMatrix_:TMatrix );
begin
     Matrix := Parent.AbsoMatrix.Inverse * GlobalMatrix_;
end;

//------------------------------------------------------------------------------

function TDrawNode.GetPosition :TPointF;
begin
     Result.X := Matrix.m31;
     Result.Y := Matrix.m32;
end;

procedure TDrawNode.SetPosition( const Position_:TPointF );
var
   M :TMatrix;
begin
     M := Matrix;

     M.m31 := Position_.X;
     M.m32 := Position_.Y;

     Matrix := M;
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

//------------------------------------------------------------------------------

function TDrawNode.GetStroke :TStrokeBrush;
begin
     if Assigned( _Stroke ) then Result :=       _Stroke
                            else Result := Parent.Stroke;
end;

procedure TDrawNode.SetStroke( const Stroke_:TStrokeBrush );
begin
     _Stroke := Stroke_;
end;

//------------------------------------------------------------------------------

function TDrawNode.GetFiller :TBrush;
begin
     if Assigned( _Filler ) then Result :=       _Filler
                            else Result := Parent.Filler;
end;

procedure TDrawNode.SetFiller( const Filler_:TBrush );
begin
     _Filler := Filler_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawNode.DrawBegin( const Canvas_:TCanvas );
begin
     _State.Assign( Canvas_ );

     with Canvas_ do
     begin
          MultiplyMatrix( Self.Matrix );

          if Assigned( _Stroke ) then Stroke.Assign( _Stroke );
          if Assigned( _Filler ) then Fill  .Assign( _Filler );
     end;
end;

procedure TDrawNode.DrawMain( const Canvas_:TCanvas );
begin

end;

procedure TDrawNode.DrawEnd( const Canvas_:TCanvas );
var
   I :Integer;
begin
     for I := 0 to ChildsN-1 do Childs[ I ].Draw( Canvas_ );

     Canvas_.Assign( _State );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawNode.Create;
begin
     inherited;

     _State := TCanvasSaveState.Create;
end;

procedure TDrawNode.AfterConstruction;
begin
     inherited;

     Area := TSingleArea2D.Create( -1, -1, +1, +1 );

     Opacity := 1;

     Stroke := nil;
     Filler := nil;
end;

destructor TDrawNode.Destroy;
begin
     if Assigned( _Stroke ) then _Stroke.Free;
     if Assigned( _Filler ) then _Filler.Free;

     _State.Free;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawNode.Draw( const Canvas_:TCanvas );
begin
     DrawBegin( Canvas_ );
     DrawMain ( Canvas_ );
     DrawEnd  ( Canvas_ );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawRoot

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawRoot.GetMatrix :TMatrix;
begin
     Result := TMatrix.Identity;
end;

procedure TDrawRoot.SetMatrix( const Matrix_:TMatrix );
begin

end;

//------------------------------------------------------------------------------

function TDrawRoot.GetAbsoMatrix :TMatrix;
begin
     Result := Matrix;
end;

procedure TDrawRoot.SetAbsoMatrix( const GlobalMatrix_:TMatrix );
begin

end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawRoot.DrawBegin( const Canvas_:TCanvas );
begin

end;

procedure TDrawRoot.DrawMain( const Canvas_:TCanvas );
begin

end;

procedure TDrawRoot.DrawEnd( const Canvas_:TCanvas );
var
   I :Integer;
begin
     for I := 0 to ChildsN-1 do Childs[ I ].Draw( Canvas_ );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawRoot.Create;
begin
     inherited;

end;

procedure TDrawRoot.AfterConstruction;
begin
     inherited;

end;

destructor TDrawRoot.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScene

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawScene.GetStroke :TStrokeBrush;
begin
     Result := _Stroke;
end;

//------------------------------------------------------------------------------

function TDrawScene.GetFiller :TBrush;
begin
     Result := _Filler;
end;

//------------------------------------------------------------------------------

function TDrawScene.GetBackColor :TAlphaColor;
begin
     Result := _BackColor;
end;

procedure TDrawScene.SetBackColor( const BackColor_:TAlphaColor );
begin
     _BackColor := BackColor_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawScene.DrawBegin( const Canvas_:TCanvas );
begin
     inherited;

     with Canvas_ do
     begin
          Stroke.Assign( _Stroke );
          Fill  .Assign( _Filler );
     end;
end;

procedure TDrawScene.DrawMain( const Canvas_:TCanvas );
begin
     inherited;

     Canvas_.Clear( _BackColor );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawScene.Create;
begin
     inherited;

end;

procedure TDrawScene.AfterConstruction;
begin
     inherited;

     Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColors.Black );
     Filler := TBrush      .Create( TBrushKind.Solid, TAlphaColors.White );

     Stroke.Join := TStrokeJoin.Round;
     Stroke.Thickness := 0.02;

     BackColor := TAlphaColors.White;
end;

destructor TDrawScene.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawShape

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawShape.GetMatrix :TMatrix;
begin
     Result := _Matrix;
end;

procedure TDrawShape.SetMatrix( const Matrix_:TMatrix );
begin
     _Matrix := Matrix_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawShape.Create;
begin
     inherited;

end;

procedure TDrawShape.AfterConstruction;
begin
     inherited;

     Matrix := TMatrix.Identity;
end;

destructor TDrawShape.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCamera

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawCamera.Create;
begin
     inherited;

end;

procedure TDrawCamera.AfterConstruction;
begin
     inherited;

     Area := TSIngleArea2D.Create( -10, -10, +10, +10 );
end;

destructor TDrawCamera.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawCamera.Render( const Canvas_:TCanvas );
begin
     Canvas_.MultiplyMatrix( AbsoMatrix.Inverse );

     ( Self.RootNode as TDrawScene ).Draw( Canvas_ );
end;

end. //######################################################################### ■
