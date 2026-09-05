Ziel des Projekts war es, mittels quantitativer Inhaltsanalyse mit BERT die Themenschwerpunktsetzung von Kandidierenden
auf Instagram in der Phase des Bundestags-Wahlkampfes zu analysieren.

Grobes Vorgehen: https://social-media-lab.net/

Beschreibung der Ordner:
1. ocr: Texte aus Posts extrahieren
Files: "OCR" und "Colab_OCR". 
Benötigte Datensätze: "data_btw_final_ocr_FALSCH.csv", "data_btw_final.csv", "ocr_colab".
Hier wurden mittels OCR texte aus den Bildern extrahiert. Da das sehr viel Zeit benötigte, wurde das gleichzeitig auf colab und stationär betrieben (deshalb zwei Dateien).
Bei der Zusammenlegung ist ein Fehler passiert, der in der Datei "OCR" behoben wurde.
Ergebender Datensatz: "data_btw_final_ocr.csv".

2. data_cleaning: Zusammenlegen der Texte mit den Captions + Datencleaning + Stichprobe für manuelle Vercodung ziehen (alles stationär)
File: "Daten_Cleaning"
Benötigter Datensatz: "data_btw_final_ocr.csv"
Die Texte (aus OCR) und Captions werden zusammengelegt, fürs weitere Vorgehen werden Fälle ausgeschlossen und Variablen gebildet, die Texte werden minimal gereinigt und eine (stratifizierte) Stichprobe gezogen.
Die manuell klassifizierten Stichproben werden ausgewertet (interrater reliabilität) und die ungleich klassifizierten Fälle ausgesucht (und bereinigt)
Ergebende Datensätze: "kandis_cleaned.csv", "Excel_eigencodierung.xlsx","Excel_eigencodierung_Lucas.xlsx", "Excel_eigencodierung_Phillip.xlsx", "Excel_eigencodierung_final.xlsx"

3. bert_process: Feinjustierung, training und Anwendung des BERT-Modells (alles in colab)
Files: "BERT_optimierung", "BERT_training", "BERT_Anwendung"
Benötigte Datensätze: "kandis_cleaned.csv", "Excel_eigencodierung.xlsx"
Mit den manuell klassifizierten Fällen werden die Hyperparameter des BERT-Modells optimiert, dann wird das BERT-Modell mit den optimierten Hyperparametern an den manuell klassifizierten Fällen feinjustiert und dann der Datensatz mit dem feinjustierten Modell klassifiziert.
Ergebender Datensatz: "kandis_vercodet.csv"

4. manifesto_add_on: Ergänzung des Datensatzes um kleinere Infos und Kombination mit Manifesto-Daten (alles stationär)
Files: "BERT_kandis_partei_kombi"
benötigte Datensätze: "kandis_vercodet.csv", "übersicht_kandis.csv", "MPD_data.csv"
Aus dem amtlichen Datensatz werden zum Datensatz mit Posts als Reihen ein paar infos dazugenommen
Der Ausgangsdatensatz (kandis_vercodet) wird so transformiert, dass für die drei benötigten Zeiträume (vor Veröffentlichung der Wahlprogramme, danach, gesamter Zeitraum) die Themenanteile in der Issue-Kommunikation der Kandidierenden als Reihen vorliegen.
Ergebende Datensätze: "kandis_uncombined.csv" 

5. analysis: Auswertung (in R)
File: "BERT_Regressionen"
benötigte Datensätze: "kandis_uncombined.csv", "kandis_party_merge.csv", "kandis_party_merge_nach.csv", "kandis_party_merge_vor.csv"
Die Daten werden explorativ und mit Regressionen ausgewertet
