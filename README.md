# WeatherDashboard

Für die Live-Wetterdaten wird ein API-Key von [Openweathermap](https://openweathermap.org/) benötigt.

Um Daten für weitere Stationen hinzuzufügen, kann ein Datensatz vom [Climate data Center](https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/) des DWD heruntergeladen werden. Aus der Zip-Datei ist die Txt-Datei (produkt_klima_tag_<...>.txt) in das Projektverzeichnis zu extrahieren. Zudem muss die Datei stations.geojson, wie auch die Liste file_paths in Zeile 663 (Script.R) entsprechend ergänzt werden.  
