unit hiscore;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,snake, ExtCtrls;

type
  TForm4 = class(TForm)
    ListBox_name: TListBox;
    ListBox_score: TListBox;
    Button1: TButton;
    Image1: TImage;
    ListBox_level: TListBox;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);

  procedure formcreate(sender: TObject);
  procedure blank_hiscores_table(); //efface le tableau score
  procedure save_hiscore_table(); //sauvegarde score dans fichier texte
  procedure test_score(sc:integer;var iin:boolean); //verifie si le score est inclu dans les hiscores
  procedure incrementer(sc:integer);//decale les hiscores
  procedure afficher();//affiche les scores dans les listboxs
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form4: TForm4;

implementation

uses nom;


{$R *.dfm}
const
max_hiscores=10; //tableau de 10 lignes
type
hiscores_type= array[1..max_hiscores] of record
player_name:string;
score:integer;
niveau:integer;  //pas tres utile!!
end;
var
hiscores_table:hiscores_type;
hiscore_filename:string;
Nouveau_score: boolean;
indice:integer;
//........................................................
 procedure TForm4.blank_hiscores_table();
 var x:integer;
 begin
 for x:=1 to max_hiscores do
 begin
 hiscores_table[x].player_name:='';
 hiscores_table[x].score:=0;
 hiscores_table[x].niveau:=0;
 end;
 end;
//..............................................................

  procedure TForm4.formcreate(sender: TObject);
 var

 infile:textfile;
 x:integer;
  begin


 Nouveau_score:=false;
 indice:=0;
 blank_hiscores_table;
 hiscore_filename:=changefileext(Application.ExeName, '.px');
 if fileexists(hiscore_filename)then
 begin
 assignfile(infile,hiscore_filename);
 reset(infile);
 for x:=1 to max_hiscores do
 begin
readln(infile,hiscores_table[x].player_name);
readln(infile,hiscores_table[x].score);
readln(infile,hiscores_table[x].niveau);
 end;
 closefile(infile);
 end;

  end;

//...................................................
 procedure TForm4.save_hiscore_table();
 var outfile:textfile;
 x:integer;
 begin
  assignfile(outfile,hiscore_filename);
  rewrite(outfile);
  for x:=1 to max_hiscores do
  begin
  writeln(outfile,hiscores_table[x].player_name);
    writeln(outfile,hiscores_table[x].score);
    writeln(outfile,hiscores_table[x].niveau);
    end;
    closefile(outfile);
    end;
//.......................................................

 procedure TForm4.test_score(sc:integer;var iin:boolean);
 var i:integer;
 begin

 for i:=10 downto 1 do
 begin
 if sc> hiscores_table[i].score  then  //sc = score
  begin indice:=i;
  iin:=true;
  end;
   end;
   end;
 //...............................................................
  procedure TForm4.incrementer(sc:integer);
  var i,nb:integer;
  nm:string;
  begin
  nb:=sc;
 
 nm:=(Form7.edit1.text);
  Nouveau_score:=true;
  if (indice>0) and  (Nouveau_score=true) then begin
    for i:=10 downto indice+1 do
    begin
   hiscores_table[i].score:=hiscores_table[i-1].score ;
   hiscores_table[i].player_name:=hiscores_table[i-1].player_name ;
   hiscores_table[i].niveau:=hiscores_table[i-1].niveau ;
      end;
   hiscores_table[indice].score:=nb ;
   hiscores_table[indice].player_name:=nm ;
   hiscores_table[indice].niveau :=level;
   save_hiscore_table;
   afficher;
      end;

   end;
 //........................................................
   procedure TForm4.afficher();
   var i:integer;
   begin
   listbox_name.clear;
   listbox_score.Clear;
   listbox_level.Clear;
   for i:=1 to 10 do begin
        listbox_name.items.add(hiscores_table[i].player_name);
        listbox_score.items.add(inttostr(hiscores_table[i].score));
        listbox_level.items.add(inttostr(hiscores_table[i].niveau));
         end;
          Form3.Enabled:=false; //desactive snake tant que hiscore est ouvert
      end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  blank_hiscores_table();
  save_hiscore_table();
   Form3.Enabled:=true;
  Form4.Close;
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
Form3.Enabled:=true;
Form4.Close;
end;

end.
