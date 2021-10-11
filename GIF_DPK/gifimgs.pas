unit GifImgs;

interface

uses
  Classes,         { Imports TComponent }
  ColorTbl,        { Imports TColorTable }
  DynArrB,         { Imports TByteArray2D }
  ExtCtrls,        { Imports TImage }
  Forms,           { Imports Application }
  GifUnit,         { Imports TGifFile }
  Graphics;        { Imports TBitmap }

type
  TGifImage = class(TImage)
  private
    Freeing: Boolean;
    NoRepeat: Boolean;
    FGifFile: TGifFile;
    FTimer: TTimer;
    FCurrSubImageNo: Integer;
    FStartImmediately: Boolean;
    destructor Destroy; override;
    procedure NextImage;
    procedure NextImageEvent(Sender: TObject);
    function  GetAnimating: Boolean;
    procedure SetAnimating(NewValue: Boolean);
  public
    constructor Create(AnOwner: TComponent); override;
    procedure AnimateOnce;
    procedure LoadFromGifFile(GifFilename: String);
    procedure LoadFromBmpFile(BmpFilename: String);
    procedure SaveToFile(Filename: String);
    procedure Slower;
    property Animating: Boolean read GetAnimating write SetAnimating;
  published
    property StartImmediately: Boolean read FStartImmediately
                                       write FStartImmediately default False;
  end; { TGifImage }

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Standard', [TGifImage])
end;

constructor TGifImage.Create(AnOwner: TComponent);
begin { TGifImage.Create }
  inherited Create(AnOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := NextImageEvent;
  Freeing := False;
  FStartImmediately := False;
  NoRepeat := False;
end;  { TGifImage.Create }

destructor TGifImage.Destroy;
begin { TGifImage.Destroy }
  FTimer.Enabled := False;
  FTimer.Free;
  FGifFile.Free;
  inherited Destroy;
end;  { TGifImage.Destroy }

procedure TGifImage.AnimateOnce;
begin { TGifImage.AnimateOnce }
  if not NoRepeat
  then begin
    NoRepeat := True;
    Animating := True;
  end;
end;  { TGifImage.AnimateOnce }

procedure TGifImage.NextImageEvent(Sender: TObject);
begin { TGifImage.NextImageEvent }
  if not Freeing
  then NextImage;
end;  { TGifImage.NextImageEvent }

procedure TGifImage.NextImage;
var SubImage: TGifSubImage;
begin { TGifImage.NextImage }
  Inc(FCurrSubImageNo);
  if FCurrSubImageNo > FGifFile.SubImages.Count
  then begin
    FCurrSubImageNo := 1;
    if NoRepeat
    then begin
      Animating := False;
      NoRepeat := False;
    end;
  end;
  SubImage := FGifFile.GetSubImage(FCurrSubImageNo);
  if FCurrSubImageNo = 1
  then Picture.Bitmap := SubImage.AsBitmap
  else with SubImage.ImageDescriptor
       do Picture.BitMap.Canvas.Draw
            (ImageLeftPos, ImageTopPos, SubImage.AsBitmap);
end;  { TGifImage.NextImage }

procedure TGifImage.LoadFromBmpFile(BmpFilename: String);
var
  Bitmap: TBitmap;
  Colormap: TColorTable;
  Pixels: TByteArray2D;
begin { TGifImage.LoadFromBmpFile }
  Freeing := True;
  Application.ProcessMessages;
  FGifFile.Free;
  FGifFile := TGifFile.Create;
  Bitmap := TBitmap.Create;
  Bitmap.LoadFromFile(BmpFilename);
  Picture.Bitmap := Bitmap;
  BitmapToPixelmatrix(Bitmap, Colormap, Pixels);
  FGifFile.AddSubImage(Colormap, Pixels);
  Freeing := False;
end;  { TGifImage.LoadFromBmpFile }

procedure TGifImage.LoadFromGifFile(GifFilename: String);
begin { TGifImage.LoadFromGifFile }
  Freeing := True;
  Application.ProcessMessages;
  FGifFile.Free;
  FGifFile := TGifFile.Create;
  FCurrSubImageNo := 1;
  FGifFile.LoadFromFile(GifFilename);
  FTimer.Enabled := False;
  if FGifFile.SubImages.Count <> 1
  then begin
    FTimer.Interval := FGifFile.AnimateInterval*10;
    if StartImmediately
    then FTimer.Enabled := True;
  end;
  Picture.Bitmap := FGifFile.GetSubImage(FCurrSubImageNo).AsBitmap;
  Freeing := False;
end;  { TGifImage.LoadFromGifFile }

procedure TGifImage.SaveToFile(Filename: String);
begin { TGifImage.SaveToFile }
  FGifFile.SaveToFile(Filename)
end;  { TGifImage.SaveToFile }

procedure TGifImage.Slower;
begin { TGifImage.Slower }
  FTimer.Interval := FTimer.Interval * 2;
end;  { TGifImage.Slower }

function TGifImage.GetAnimating: Boolean;
begin { TGifImage.GetAnimating }
  Result := FTimer.Enabled;
end;  { TGifImage.GetAnimating }

procedure TGifImage.SetAnimating(NewValue: Boolean);
begin { TGifImage.SetAnimating }
  if FTimer.Enabled <> NewValue
  then FTimer.Enabled := NewValue
end;  { TGifImage.SetAnimating }

end.  { unit GifImgs }
