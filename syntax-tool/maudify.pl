#!/usr/bin/perl -w
#
# Perl-Skript zur Konvertierung eines Core-Erlang Modules (so wie es
# der Erlang Compiler ausgibt) in die von Maude lesbare Syntax.

@infile = <STDIN>;

print("(add-module((");
foreach $line (@infile)
{
  $line_new = $line;
  
  # entferne Kommentarzeilen
  $line_new =~ s/%%.*//sg;
  
  # Der Doppelpunkt ist ein Trennzeichen, das vom Maude-Parser nicht
  # erkannt wird. Daher werden hier Leerzeichen eingefügt. Nur, falls
  # es direkt zwischen zwei einzelnen Hochkommata steht.
  $line_new =~ s/':'/' : '/g;   
  
  # Der Schrägstrich trennt Funktionsnamen von der Angabe der Stelligkeit;
  # auch ihn erkennt Maude nicht als Trennzeichen. 
  $line_new =~ s/'\//' \/ /g;
 
  # Die spitzen Klammern (im Gegensatz zu "(","[" und "{") werden auch
  # nicht erkannt. Hier kann es durch die Verwendung der regulären Ausdrücke zu Fehlern
  # kommen; z.B. werden arithmetische Vergleichssymbole hier getrennt...
  $line_new =~ s/</ < /g;
  $line_new =~ s/>/ > /g;
  $line_new =~ s/' > ='/'>='/g;
  $line_new =~ s/'= <'/'=<'/g;
  $line_new =~ s/- >/->/g;

  # Der gerade vertikale Strich als Trennsymbol in der Listendarstellung ist
  # auch unbekannt (wird nicht als Trennzeichen angenommen)
  $line_new =~ s/\|/ \| /g;
  
  # Die Darstellung für die leere ordered Sequence muss zusammengezogen
  # (ohne Leerzeichen zwischen den Klammern) erscheinen.
  $line_new =~ s/<\s*>/<>/g;

  $line_new =~ s/(.*-|.*)/$1/sg;
   
  print($line_new);
}
print(")))");

exit(0);
