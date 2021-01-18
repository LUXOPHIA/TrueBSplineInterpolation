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
       procedure SetArea( const Area_:TSingleArea2D );
       function GetMinX :Single;
       procedure SetMinX( const MinX_:Single );
       function GetMaxX :Single;
       procedure SetMaxX( const MaxX_:Single );
       function GetMinY :Single;
       procedure SetMinY( const MinY_:Single );
       function GetMaxY :Single;
       procedure SetMaxY( const MaxY_:Single );
       function GetCentX :Single;
       procedure SetCentX( const CentX_:Single );
       function GetCentY :Single;
       procedure SetCentY( const CentY_:Single );
       function GetSizeX :Single;
       procedure SetSizeX( const SizeX_:Single );
       function GetSizeY :Single;
       procedure SetSizeY( const SizeY_:Single );
       function GetMatrix :TMatrix; virtual; abstract;
       procedure SetMatrix( const Matrix_:TMatrix ); virtual; abstract;
       function GetAbsoMatrix :TMatrix; virtual;
       procedure SetAbsoMatrix( const GlobalMatrix_:TMatrix );
       function GetPosition :TPointF;
       procedure SetPosition( const Position_:TPointF );
       function GetOpacity :Single;
       procedure SetOpacity( const Opacity_:Single );
       ///// メソッド
       procedure UpdateArea; virtual;
       procedure DrawBegin( const Canvas_:TCanvas ); virtual;
       procedure DrawMain( const Canvas_:TCanvas ); virtual;
       procedure DrawEnd( const Canvas_:TCanvas ); virtual;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Area       :TSingleArea2D read GetArea       write SetArea      ;
       property MinX       :Single        read GetMinX       write SetMinX      ;
       property MaxX       :Single        read GetMaxX       write SetMaxX      ;
       property MinY       :Single        read GetMinY       write SetMinY      ;
       property MaxY       :Single        read GetMaxY       write SetMaxY      ;
       property CentX      :Single        read GetCentX      write SetCentX     ;
       property CentY      :Single        read GetCentY      write SetCentY     ;
       property SizeX      :Single        read GetSizeX      write SetSizeX     ;
       property SizeY      :Single        read GetSizeY      write SetSizeY     ;
       property Matrix     :TMatrix       read GetMatrix     write SetMatrix    ;
       property Position   :TPointF       read GetPosition   write SetPosition  ;
       property AbsoMatrix :TMatrix       read GetAbsoMatrix write SetAbsoMatrix;
       property Opacity    :Single        read GetOpacity    write SetOpacity   ;
       property Stroke     :TStrokeBrush  read   _Stroke                        ;
       property Filler     :TBrush        read   _Filler                        ;
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
       ///// アクセス
       ///// メソッド
     public
       constructor Create; override;
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
     _Area := Area_;  UpdateArea;
end;

function TDrawNode.GetMinX :Single;
begin
     Result := _Area.Min.X;
end;

procedure TDrawNode.SetMinX( const MinX_:Single );
begin
     _Area.Min.X := MinX_;  UpdateArea;
end;

function TDrawNode.GetMaxX :Single;
begin
     Result := _Area.Max.X;
end;

procedure TDrawNode.SetMaxX( const MaxX_:Single );
begin
     _Area.Max.X := MaxX_;  UpdateArea;
end;

function TDrawNode.GetMinY :Single;
begin
     Result := _Area.Min.Y;
end;

procedure TDrawNode.SetMinY( const MinY_:Single );
begin
     _Area.Min.Y := MinY_;  UpdateArea;
end;

function TDrawNode.GetMaxY :Single;
begin
     Result := _Area.Max.Y;
end;

procedure TDrawNode.SetMaxY( const MaxY_:Single );
begin
     _Area.Max.Y := MaxY_;  UpdateArea;
end;

function TDrawNode.GetCentX :Single;
begin
     Result := _Area.CentX;
end;

procedure TDrawNode.SetCentX( const CentX_:Single );
begin
     _Area.CentX := CentX_;  UpdateArea;
end;

function TDrawNode.GetCentY :Single;
begin
     Result := _Area.CentY;
end;

procedure TDrawNode.SetCentY( const CentY_:Single );
begin
     _Area.CentY := CentY_;  UpdateArea;
end;

function TDrawNode.GetSizeX :Single;
begin
     Result := _Area.SizeX;
end;

procedure TDrawNode.SetSizeX( const SizeX_:Single );
begin
     _Area.SizeX := SizeX_;  UpdateArea;
end;

function TDrawNode.GetSizeY :Single;
begin
     Result := _Area.SizeY;
end;

procedure TDrawNode.SetSizeY( const SizeY_:Single );
begin
     _Area.SizeY := SizeY_;  UpdateArea;
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

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawNode.UpdateArea;
begin

end;

//------------------------------------------------------------------------------

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

/////////////////////////////////////////////////////////////////////// メソッド

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawCamera.Create;
begin
     inherited;

     MinX := -10;  MaxX := +10;
     MinY := -10;  MaxY := +10;
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
