unit snake;

interface

uses
 Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, MPlayer, XPMan, MMSystem,ImgList,
  apropos,  Menus, GifImgs, jpeg;

type
  TForm3 = class(TForm)
    Timer_affectercases: TTimer;
    Timer_avancer: TTimer;
    Label1: TLabel;
    score: TLabel;
    Shape1: TShape;
    MainMenu1: TMainMenu;
    Jouer1: TMenuItem;
    Rejouer1: TMenuItem;
    Quitter1: TMenuItem;
    Apropos1: TMenuItem;
    reglesdujeu1: TMenuItem;
    label2: TLabel;
    hiscore1: TMenuItem;
    ImageList_serpent: TImageList;
    N1: TMenuItem;
    options1: TMenuItem;
    Difficult1: TMenuItem;
    Facile1: TMenuItem;
    Moyen1: TMenuItem;
    Difficile1: TMenuItem;
    Shape2: TShape;
    ImageList_obstacle: TImageList;
    Label_niveau: TLabel;
    Timer_niveau: TTimer;
    niveau1: TMenuItem;
    prairie1: TMenuItem;
    desert1: TMenuItem;
    N3piscine1: TMenuItem;
    N4papillon1: TMenuItem;
    N5lenfer1: TMenuItem;
    ImageList_cibles: TImageList;
    Shape3: TShape;
    Image2: TImage;
    N6leglacier1: TMenuItem;
    procedure N6leglacier1Click(Sender: TObject);
    procedure N5lenfer1Click(Sender: TObject);
    procedure N4papillon1Click(Sender: TObject);
    procedure N3piscine1Click(Sender: TObject);
    procedure desert1Click(Sender: TObject);
    procedure prairie1Click(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure hiscore1Click(Sender: TObject);
    procedure reglesdujeu1Click(Sender: TObject);
    procedure Quitter1Click(Sender: TObject);
    procedure Rejouer1Click(Sender: TObject);
     procedure Facile1Click(Sender: TObject);
    procedure Difficile1Click(Sender: TObject);
    procedure Moyen1Click(Sender: TObject);
    procedure Afficher_niveau(sender:tobject);

procedure oncreate(Sender: TObject);  //creation
procedure affectercases(Sender:TObject);  //creation du plan et position de base du serpent
procedure FormKeyDown(Sender: TObject;var Key: Word;Shift: TShiftState);
procedure check(); //affiche un objet en fonction de la valeur pos de la case
procedure decalage(); //decale le serpent dans la direction souhaitee
procedure avancer(Sender :TObject); //decale la tete du serpent
procedure placerpom();// place la pomme sur le plan
procedure checkpom(); //verifie la presence d'une pomme ou du corps du serpent
procedure perdu(); //verifie les score, arrete le jeu,
                  //affiche perdu et au besoin demande le nom
                  //pour les hiscores.





  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;


  type
 rama=record
  t:trect;
  pos:integer;
  lettre:char;
  end;


   color=record   // type de couleur pour le fond
   col:integer;
   end;

  directiontype=(dright,dleft,dup,ddown);


var
Form3: TForm3;
a:array[1..50,1..50] of rama;      //tableau du plan
serpent:array[1..154,1..2] of integer;  //serpent
b:array[1..10] of color;//tableau des couleurs pour le fond
n,imax,jmax:integer;
key:word;
abs,ord,long:integer;   //caracteristiques du serpent
direction:directiontype;
position:Tposition;
pomme:integer;   //score
nb_pomme:integer; //nb de cibles atteintes
pause,iin:boolean;
level:integer;
level_filename:string;

implementation

uses nom, hiscore,regles, perdu;

{$R *.dfm}
procedure TForm3.oncreate(Sender: TObject);
begin
iin:=false;
 form3.canvas.Brush.Color:=clred;
randomize;
   abs:=5;
   ord:=1;
   imax:=30;
   jmax:=25;
   long:=5;               //longueure initiale du serpent
   pomme:=0;     //score (en fct de la difficulte)
   nb_pomme:=0;//nb de pommes
   pause:=false;
   level:=1;
    b[1].col:=($00B0F7CD); //couleur niveau 1  prairie
    b[2].col:=($007DEFF4); //idem niv 2  desert
    b[3].col:=($f4dca2);  //  piscine
    b[4].col:=($b3e0ed) ; // papillon
    b[5].col:=(clblack); //enfer
    b[6].col:=($efe5ce) ; // glace
 end;

procedure TForm3.affectercases(Sender:TObject);
   var t:trect;
       i,j:integer;
        temp:char;
 infile:textfile;




begin
  shape2.Brush.color:=b[level].col;//couleur du fond
  timer_niveau.Enabled:=false;
  label_niveau.Visible:=false;

 direction:=dright;    //premiere direction : droite

 //on va ouvrir les fichiers .level et les lire
 //les x correspondent au vide donc a pos=2
 //les o a des obstacles donc a pos=5
 level_filename:=changefileext(application.ExeName,'.level'+inttostr(level));
 if fileexists(level_filename)then
 begin
 assignfile(infile,level_filename);
 reset(infile);

 i:=1;j:=1;
 while  not eof(infile) do begin
  read(infile,temp);
 if temp=#13 then begin
  j:=j+1;i:=1;end
  else if  (temp='x') or (temp='o')  then begin
  a[i,j].lettre:=temp;
  i:=i+1;end else if
   temp<>#10 then i:=i+1;

  end;
closefile(infile);
  end;



t.Left:=5;

for i:=1 to imax do
begin
t.top:=5;
  for j:=1 to jmax do
  begin
   t.bottom:= t.Top+20;   //case de taille 20 x 20
   t.right:=t.Left+20;
   a[i,j].t:=t;
   form3.canvas.Brush.Color:=clwhite;
   t.top:=t.top+22;
  if  a[i,j].lettre='x' then
   a[i,j].pos:=2 else
    a[i,j].pos:=5;   //mur
   end;

t.left:=t.Left+22;
end;
serpent[1,1]:=abs;     //position de la tete du serpent initialement
serpent[1,2]:=ord;

   for i:=2 to long do
   begin
  serpent[i,1]:=abs-i+1;
  serpent[i,2]:=ord;
  a[abs,ord].pos:=4; //pos = 4 : la tete
  a[abs-i+1,ord].pos:=1;   //pos =1 : le coprs
   end;
 timer_affectercases.enabled:=false;     //une fois que les cases sont
                                          // affectees on arrete la boucle
                                           //qui n'en est pas une du coup ....
Timer_avancer.Enabled:=true;       //declenche la procedure avancer
placerpom;           //explicite....

end;



//.........................................................................;
procedure TForm3.decalage();
var i:integer;
begin
    a[serpent[long,1],serpent[long,2]].pos:=2; //la derniere case du serpent devient vide
  for i:=long downto 2 do        //decalage des coordonnees du coprs
     begin
  serpent[i,1]:=serpent[i-1,1];
  serpent[i,2]:=serpent[i-1,2];
  a[serpent[i,1],serpent[i,2]].pos:=1;
     end;
end;


//......................................................................
procedure TForm3.FormKeyDown(Sender: TObject;var Key: Word;Shift: TShiftState);
begin

if ((key=vk_right) and (direction<>dleft))  then
  begin
 direction:=dright;
   end;

if ((key=vk_down) and (direction<>dup))  then
  begin
   direction:=ddown;
   end;

if ((key=vk_left)and (direction<>dright))  then
  begin
   direction:=dleft;
  end;

if ((key=vk_up) and (direction<>ddown))  then
  begin
   direction:=dup;
  end;

if key=vk_space then   // une fonction arret pause
     if pause=false then begin pause:=true; Timer_avancer.enabled:=false;label2.Visible:=true;end
      else begin pause:=false;Timer_avancer.enabled:=true;label2.visible:=false;end;


if key=vk_escape then     //quitter l'application
  application.Terminate;


end;



//..........................................................................
procedure TForm3.check();
var i,j:integer;
begin

   for i:=1 to imax do
   begin
   for j:=1 to jmax do
   begin

   case  a[i,j].pos of
   1: begin  //pos=1 : serpent corps
   ImageList_serpent.Draw(Form3.Canvas, a[i,j].t.left, a[i,j].t.top, 4);end
   ;
   4: begin       //pos=4 tete du serpent
      case direction of //charge les differentes tetes
      dup:begin ImageList_serpent.Draw(Form3.Canvas, a[i,j].t.left, a[i,j].t.top, 3);end;
      ddown:begin ImageList_serpent.Draw(Form3.Canvas, a[i,j].t.left, a[i,j].t.top, 0);end;
      dright : begin ImageList_serpent.Draw(Form3.Canvas, a[i,j].t.left, a[i,j].t.top, 1);end;
      dleft :begin ImageList_serpent.Draw(Form3.Canvas, a[i,j].t.left, a[i,j].t.top, 2);end;
                       end;
      ImageList_serpent.BkColor:=b[level].col; //fond de la meme couleur que le plateau
                  end;



   2:begin
    form3.canvas.Brush.Color:=b[level].col; //pos=2:  vide
    form3.Canvas.FillRect(a[i,j].t);
      end;

   3:begin    //cibles changent en fct du niveau
   ImageList_cibles.Draw(Form3.Canvas, (a[i,j].t.left), (a[i,j].t.top), level);
   ImageList_cibles.BkColor:=b[level].col;
     end;


    5: begin  //les murs ou obstacles
    ImageList_obstacle.Draw(Form3.Canvas, a[i,j].t.left, a[i,j].t.top, level);
    Imagelist_obstacle.BkColor:=b[level].col;//change les murs en fct du niveau
    end;

   end;
   end;
   end;
 end;


//............................................................................
  procedure TForm3.avancer(Sender :TObject);
begin

  decalage;      //decale le corps
  case direction of          //decale la tete

dright : begin
  if serpent[1,1]<>imax then begin
  serpent[1,1]:=serpent[1,1]+1;
  checkpom; //verifie la presence d'une pomme ou du corps
  a[serpent[1,1],serpent[1,2]].pos:=4;
  check;  // update l'etat de l'ecran
   end else perdu;
  end;

ddown : begin
   if serpent[1,2]<>jmax then begin
   serpent[1,2]:=serpent[1,2]+1;
   checkpom; //verifie la presence d'une pomme ou du corps
   a[serpent[1,1],serpent[1,2]].pos:=4;
   check;  // update l'etat de l'ecran
  end else perdu;
   end;

dleft : begin
   if serpent[1,1]<>1 then begin
   serpent[1,1]:=serpent[1,1]-1;
   checkpom; //verifie la presence d'une pomme ou du corps
  a[serpent[1,1],serpent[1,2]].pos:=4;
  check;  // update l'etat de l'ecran
   end else perdu;
  end;

dup: begin
   if serpent[1,2]<>1 then begin
    serpent[1,2]:=serpent[1,2]-1;
    checkpom; //verifie la presence d'une pomme ou du corps
    a[serpent[1,1],serpent[1,2]].pos:=4;
    check;  // update l'etat de l'ecran
    end else perdu;
   end;
 end;
end;

//............................................................
procedure Tform3.perdu();
begin
Timer_avancer.Enabled:=false;    //arrete la boucle principale
  Form2.visible:=true;          //affiche perdu
  Form2.Timer1.enabled:=true;    //perdu disparait au bout de 3 sec
  Form4.test_score(pomme,iin); //test si le score est un HIGH SCORE
   if (iin=true) and (pomme>0) then
   Form7.visible:=true;   //demande le nom du joueur
end;

//................................................................
procedure Tform3.placerpom();
var c,b:integer;
begin
   randomize;
 c:=Random(30);
 b:=Random(25);

   if a[c,b].pos=2 then       //si la case est vide
   begin
   a[c,b].pos:=3;             //alors pomme
   ImageList_serpent.Draw(Form3.Canvas, a[c,b].t.left, a[c,b].t.top, 5);
   end else placerpom;      //sinon on recommence
end;



//..........................................................
procedure TForm3.checkpom();
begin
       if a[serpent[1,1],serpent[1,2]].pos=3 then  //si pomme
       begin
       a[serpent[1,1],serpent[1,2]].pos:=4;

     if Facile1.Checked=true then pomme:=pomme+1;  //facile donne 1 points
     if Difficile1.Checked=true then pomme:=pomme+3;//difficile 3
     if Moyen1.Checked=true then pomme:=pomme+2;//et moyen 2
       nb_pomme:=nb_pomme+1;
       score.Caption:=inttostr(pomme);
       placerpom; //en place une autre
       long:=long+1;      //augment la taille du serpent
       check;
       end;

       if (a[serpent[1,1],serpent[1,2]].pos=1) or (a[serpent[1,1],serpent[1,2]].pos=5) then   //si corps
      perdu;



  if (nb_pomme=10) and (level=1)    then begin
  level:=2;          //niveau2
  Timer_avancer.Enabled:=false; //stop avancer
  abs:=10;
  long:=10;
  Timer_niveau.enabled:=true;
  timer_affectercases.Enabled:=true;
  end;

  if (nb_pomme=20) and (level=2) then begin
  level:=3;          //niveau3
  Timer_avancer.Enabled:=false; //stop avancer
  abs:=15;
  long:=15;
  Timer_niveau.enabled:=true;
  timer_affectercases.Enabled:=true;
  end;

  if (nb_pomme=30) and (level=3) then begin
  level:=4;          //niveau4
  Timer_avancer.Enabled:=false; //stop avancer
  abs:=20;
  long:=20;
  Timer_niveau.enabled:=true;
  timer_affectercases.Enabled:=true;
  end;

   if (nb_pomme=40) and (level=4) then begin
  level:=5;          //niveau5
  Timer_avancer.Enabled:=false; //stop avancer
  abs:=20;
  long:=20;
  Timer_niveau.enabled:=true;
  timer_affectercases.Enabled:=true;
  end;

    if (nb_pomme=50) and (level=5) then begin
  level:=6;          //niveau6
  Timer_avancer.Enabled:=false; //stop avancer
  abs:=20;
  long:=20    ;
  Timer_niveau.enabled:=true;
  timer_affectercases.Enabled:=true;
  end;


  if (nb_pomme=60) and (level=6) then begin
  level:=1;
  nb_pomme:=0; // on revient au niveau 1
  Timer_avancer.Enabled:=false; //stop avancer
  long:=10;
  abs:=10;
   Timer_niveau.enabled:=true;
  timer_affectercases.Enabled:=true ;
  end;


 end;




//.............................................................

procedure TForm3.Rejouer1Click(Sender: TObject);
begin
Timer_avancer.enabled:=false; //arrete la boucle
long:=5;
Timer_avancer.Interval:=200;   //vitesse ini d serpent : facile
pomme:=0;          //score = 0
nb_pomme:=0;   // nb de cibles =0
level:=1;
score.Caption:='0';               //pas de points
direction:=dright;
facile1.Checked:=true;moyen1.Checked:=false;difficile1.Checked:=false; //mode facile
Timer_affectercases.Enabled:=true;     //redessine le tableau initial
label2.Visible:=false;   //pause disparait
end;


//............................................
procedure TForm3.Quitter1Click(Sender: TObject);
begin
application.Terminate;
end;

//.....................................................
procedure TForm3.reglesdujeu1Click(Sender: TObject);
begin
Form1.Visible:=true;
end;


//......................................................
procedure TForm3.hiscore1Click(Sender: TObject);
begin
hiscore.Form4.afficher;
hiscore.Form4.Visible:=true;
end;

//..........................................................
procedure TForm3.N1Click(Sender: TObject);
begin
apropos.Form5.visible:=true;
end;

//...............................................................
//menu difficulte : seule la vitesse chgange
//..............................................................;
procedure TForm3.Moyen1Click(Sender: TObject);
begin
Facile1.Checked:=false;
Difficile1.Checked:=false;
Moyen1.Checked:=true;
timer_avancer.Interval:=150;
end;

procedure TForm3.Difficile1Click(Sender: TObject);
begin
Facile1.Checked:=false;
Difficile1.Checked:=true;
Moyen1.Checked:=false;
timer_avancer.Interval:=100;
end;

procedure TForm3.Facile1Click(Sender: TObject);
begin
Facile1.Checked:=true;
Difficile1.Checked:=false;
Moyen1.Checked:=false;
timer_avancer.Interval:=100;
end;


//..........affiche le numero du level..............
  procedure TForm3.Afficher_niveau(sender:tobject);
  begin
   Label_niveau.Caption:='Niveau '+inttostr(level)+'';
   Label_niveau.Visible:=true;
  end;



  //......................................................
  //choix du niveau diectement
  //.......................................................
procedure TForm3.prairie1Click(Sender: TObject);
begin
prairie1.Checked:=true;
N5lenfer1.Checked:=false;
desert1.Checked:=false;
N3piscine1.Checked:=false;
N4papillon1.Checked:=false;
N6leglacier1.Checked:=false;
level:=1;
end;

procedure TForm3.desert1Click(Sender: TObject);
begin
prairie1.Checked:=false;
N5lenfer1.Checked:=false;
desert1.Checked:=true;
N3piscine1.Checked:=false;
N4papillon1.Checked:=false;
N6leglacier1.Checked:=false;
level:=2;
 timer_avancer.enabled:=false;
 Timer_affectercases.Enabled:=true;     //redessine le tableau initial
end;

procedure TForm3.N3piscine1Click(Sender: TObject);
begin
 prairie1.Checked:=false;
N5lenfer1.Checked:=false;
desert1.Checked:=false;
N3piscine1.Checked:=true;
N4papillon1.Checked:=false;
N6leglacier1.Checked:=false;
level:=3;
 timer_avancer.enabled:=false;
 Timer_affectercases.Enabled:=true;     //redessine le tableau initial
end;

procedure TForm3.N4papillon1Click(Sender: TObject);
begin
prairie1.Checked:=false;
N5lenfer1.Checked:=false;
desert1.Checked:=false;
N3piscine1.Checked:=false;
N4papillon1.Checked:=true;
N6leglacier1.Checked:=false;
level:=4;
 timer_avancer.enabled:=false;
 Timer_affectercases.Enabled:=true;     //redessine le tableau initial
 end;

procedure TForm3.N5lenfer1Click(Sender: TObject);
begin
prairie1.Checked:=false;
N5lenfer1.Checked:=true;
desert1.Checked:=false;
N3piscine1.Checked:=false;
N4papillon1.Checked:=false;
N6leglacier1.Checked:=false;
level:=5;
timer_avancer.enabled:=false;
 Timer_affectercases.Enabled:=true;     //redessine le tableau initial
end;

procedure TForm3.N6leglacier1Click(Sender: TObject);
begin
prairie1.Checked:=false;
N5lenfer1.Checked:=false;
desert1.Checked:=false;
N3piscine1.Checked:=false;
N4papillon1.Checked:=false;
N6leglacier1.Checked:=true;
level:=6;
timer_avancer.enabled:=false;
 Timer_affectercases.Enabled:=true;     //redessine le tableau initial
end;

end.



