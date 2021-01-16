unit LUX.Draw.Scene;

interface //#################################################################### ■

uses System.Types, System.UITypes, System.Math.Vectors,
     FMX.Graphics,
     LUX, LUX.D1, LUX.D2, LUX.Data.Tree;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TDrawNode     = class;
       TDrawScene  = class;
       TDrawCamera = class;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawNode

     TDrawNode = class( TTreeNode<TDrawNode,TDrawNode> )
     private
       _State :TCanvasSaveState;
     protected
       _Opacity :Single;
       _Stroke  :TStrokeBrush;
       _Filler  :TBrush;
       ///// アクセス
       function GetMatrix :TMatrix; virtual; abstract;
       procedure SetMatrix( const Matrix_:TMatrix ); virtual; abstract;
       function GetAbsoMatrix :TMatrix; virtual;
       procedure SetAbsoMatrix( const GlobalMatrix_:TMatrix );
       function GetPosition :TPointF;
       procedure SetPosition( const Position_:TPointF );
       function GetOpacity :Single;
       procedure SetOpacity( const Opacity_:Single );
       ///// メソッド
       procedure DrawBegin( const Canvas_:TCanvas ); virtual;
       procedure DrawMain( const Canvas_:TCanvas ); virtual;
       procedure DrawEnd( const Canvas_:TCanvas ); virtual;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Matrix     :TMatrix      read GetMatrix     write SetMatrix    ;
       property Position   :TPointF      read GetPosition   write SetPosition  ;
       property AbsoMatrix :TMatrix      read GetAbsoMatrix write SetAbsoMatrix;
       property Opacity    :Single       read GetOpacity    write SetOpacity   ;
       property Stroke     :TStrokeBrush read   _Stroke                        ;
       property Filler     :TBrush       read   _Filler                        ;
       ///// メソッド
       procedure Draw( const Canvas_:TCanvas );
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScene

     TDrawScene = class( TDrawNode )
     private
     protected
       _BackColor :TAlphaColor;
       ///// アクセス
       function GetMatrix :TMatrix; override;
       procedure SetMatrix( const Matrix_:TMatrix ); override;
       function GetAbsoMatrix :TMatrix; override;
       function GetBackColor :TAlphaColor;
       procedure SetBackColor( const BackColor_:TAlphaColor );
       ///// メソッド
       procedure DrawBegin( const Canvas_:TCanvas ); override;
       procedure DrawMain( const Canvas_:TCanvas ); override;
       procedure DrawEnd( const Canvas_:TCanvas ); override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Matrix     :TMatrix     read GetMatrix                       ;
       property Position   :TPointF     read GetPosition                     ;
       property AbsoMatrix :TMatrix     read GetAbsoMatrix                   ;
       property BackColor  :TAlphaColor read GetBackColor  write SetBackColor;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawShape

     TDrawShape = class( TDrawNode )
     private
     protected
       _Matrix  :TMatrix;
       ///// アクセス
       function GetMatrix :TMatrix; override;
       procedure SetMatrix( const Matrix_:TMatrix ); override;
       ///// メソッド
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       ///// メソッド
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawCamera

     TDrawCamera = class( TDrawShape )
     private
     protected
       _SizeW :Single;
       _SizeH :Single;
       ///// アクセス
       function GetSizeW :Single;
       procedure SetSizeW( const SizeW_:Single );
       function GetSizeH :Single;
       procedure SetSizeH( const SizeH_:Single );
       ///// メソッド
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property SizeW :Single read GetSizeW write SetSizeW;
       property SizeH :Single read GetSizeH write SetSizeH;
       ///// メソッド
       procedure Render( const Canvas_:TCanvas );
     end;

implementation //############################################################### ■

uses System.Math;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawNode

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

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

     _Stroke := TStrokeBrush.Create( TBrushKind.Solid, TAlphaColors.Null );
     _Filler := TBrush      .Create( TBrushKind.Solid, TAlphaColors.Null );

     _Opacity     := 1;
     _Stroke.Join := TStrokeJoin.Round;
     _Stroke.Thickness := 0.02;
end;

destructor TDrawNode.Destroy;
begin
     _Stroke.Free;
     _Filler.Free;

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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawScene

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDrawScene.GetMatrix :TMatrix;
begin
     Result := TMatrix.Identity;
end;

procedure TDrawScene.SetMatrix( const Matrix_:TMatrix );
begin

end;

//------------------------------------------------------------------------------

function TDrawScene.GetAbsoMatrix :TMatrix;
begin
     Result := Matrix;
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

end;

procedure TDrawScene.DrawMain( const Canvas_:TCanvas );
begin
     Canvas_.Clear( _BackColor );
end;

procedure TDrawScene.DrawEnd( const Canvas_:TCanvas );
var
   I :Integer;
begin
     for I := 0 to ChildsN-1 do Childs[ I ].Draw( Canvas_ );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawScene.Create;
begin
     inherited;

     _BackColor := TAlphaColors.White;
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

     _Matrix := TMatrix.Identity;
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

function TDrawCamera.GetSizeW :Single;
begin
     Result := _SizeW;
end;

procedure TDrawCamera.SetSizeW( const SizeW_:Single );
begin
     _SizeW := SizeW_;
end;

function TDrawCamera.GetSizeH :Single;
begin
     Result := _SizeH;
end;

procedure TDrawCamera.SetSizeH( const SizeH_:Single );
begin
     _SizeH := SizeH_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawCamera.Create;
begin
     inherited;

     SizeW  := 20;
     SizeH  := 20;
end;

destructor TDrawCamera.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawCamera.Render( const Canvas_:TCanvas );
begin
     with Canvas_ do
     begin
          //MultiplyMatrix( TMatrix.CreateScaling( 2 / _SizeW, 2 / _SizeH ) );

          MultiplyMatrix( AbsoMatrix.Inverse );
     end;

     ( Self.RootNode as TDrawScene ).Draw( Canvas_ );
end;

end. //######################################################################### ■
