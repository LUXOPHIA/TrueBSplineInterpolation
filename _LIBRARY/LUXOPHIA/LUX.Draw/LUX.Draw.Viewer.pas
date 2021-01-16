unit LUX.Draw.Viewer;

interface //#################################################################### ■

uses System.Types, System.UITypes, System.Classes,
     FMX.Graphics, FMX.Controls, FMX.Forms,
     LUX, LUX.D1, LUX.D2,
     LUX.Draw.Scene, FMX.Types, FMX.Objects;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TDrawViewer = class;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawViewer

     TDrawViewer = class( TFrame )
     private
       { private 宣言 }
     protected
       _Camera :TDrawCamera;
       ///// アクセス
       function GetCamera :TDrawCamera;
       procedure SetCamera( const Camera_:TDrawCamera );
       ///// メソッド
       procedure Paint; override;
     public
       { public 宣言 }
       constructor Create( Owner_:TComponent ); override;
       destructor Destroy; override;
       ///// プロパティ
       property Camera :TDrawCamera read GetCamera write SetCamera;
     end;

implementation //############################################################### ■

{$R *.fmx}

uses System.Math.Vectors;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDrawViewer

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

//------------------------------------------------------------------------------

function TDrawViewer.GetCamera :TDrawCamera;
begin
     Result := _Camera;
end;

procedure TDrawViewer.SetCamera( const Camera_:TDrawCamera );
begin
     _Camera := Camera_;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDrawViewer.Paint;
var
   V :TVector;
   S :Single;
   M :TMatrix;
begin
     inherited;

     if Assigned( _Camera ) then
     begin
          V := LocalToAbsoluteVector( Vector( Width, Height ) );

          if V.X * _Camera.SizeH <= V.Y * _Camera.SizeW then
          begin
               // ┏━━━┓
               // ┠───┨
               // ┃      ┃
               // ┠───┨
               // ┗━━━┛

               S := V.X / _Camera.SizeW;
          end
          else
          begin
               // ┏┯━┯┓
               // ┃│  │┃
               // ┃│  │┃
               // ┃│  │┃
               // ┗┷━┷┛

               S := V.Y / _Camera.SizeH;
          end;

          with M do
          begin
               m11 :=  +S  ;  m12 :=   0  ;  m13 := 0;
               m21 :=   0  ;  m22 :=  -S  ;  m23 := 0;
               m31 := V.X/2;  m32 := V.Y/2;  m33 := 1;
          end;

          Canvas.MultiplyMatrix( M );

          _Camera.Render( Canvas );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDrawViewer.Create( Owner_:TComponent );
begin
     inherited;

     AutoCapture := True;

     _Camera := nil;
end;

destructor TDrawViewer.Destroy;
begin

     inherited;
end;

end. //######################################################################### ■
