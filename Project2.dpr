program Project2;

{%Gif 'IMG\ange_rose.gif'}
{%Bmp 'IMG\arbre.bmp'}
{%Gif 'IMG\arbre.gif'}
{%Jpg 'IMG\arbre.jpg'}
{%Bmp 'IMG\arbre2.bmp'}
{%Gif 'IMG\arbre2.gif'}
{%Bmp 'IMG\arbre_026.bmp'}
{%Gif 'IMG\arbre_026.gif'}
{%Gif 'IMG\arbre_040.gif'}
{%Gif 'IMG\arraignee.gif'}
{%Gif 'IMG\banane.gif'}
{%Bmp 'IMG\flammes012.bmp'}
{%Gif 'IMG\flammes012.gif'}
{%Gif 'IMG\gopokeball.gif'}
{%Gif 'IMG\jongle.gif'}

uses
  Forms,
  snake in 'snake.pas' {Form3},
  perdu in 'perdu.pas' {Form2},
  regles in 'regles.pas' {Form1},
  nom in 'nom.pas' {Form7},
  hiscore in 'hiscore.pas' {Form4},
  apropos in 'apropos.pas' {Form5},
  colortbl in 'GIF_DPK\colortbl.pas',
  dynarrb in 'GIF_DPK\dynarrb.pas',
  fmwarerr in 'GIF_DPK\fmwarerr.pas' {WarningForm},
  gifdecl in 'GIF_DPK\gifdecl.pas',
  gifimgs in 'GIF_DPK\gifimgs.pas',
  GifUnit in 'GIF_DPK\GifUnit.pas',
  moreutil in 'GIF_DPK\moreutil.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm7, Form7);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TForm5, Form5);
  Application.CreateForm(TWarningForm, WarningForm);
  Application.Run;
end.
