#' basic concept of TimeSpatData
#' @docType package
#' @name TimeSpatData-package
#' @description
#' Die grundsätzliche Beschreibung:
#' # Zeitliche-räumliche Daten Strukturen definieren
#'
#' ## Makro Data-Dimensionen (`Dim`) definieren
#'
#' In der Makrosicht ist das TimeSpatData in drei Dimensionen definiert:
#'
#'   -   `Time`: Zeitstampel definieren
#'
#' -   `Spat`: Räumliche Struktur definieren
#'
#' -   `Vari`: mehrere Variablen (Größe) definieren
#'
#' ## Datenstruktur (Array) Dimensionen (`dim`) definieren
#'
#' Die Daten bei der Bearbeitung und Speicherung werden in Array-Form
#' gegeben, die Makro Dimensionen können nicht deutlich in eine
#' Array-Dimensionen definiert werden, deshalb die Array-Dimensionen sind
#' neu definiert:
#'
#'   ### `time`
#'
#'   Dimension `time` ist für Zeit definiert und besitzt
#' **Rückwärtskonsistenz**. Es steht immer in der ersten Dimension, mit
#' zwei Möglickeiten:
#'
#'   -   `continuous`: mit festem Zeitschritt, bei `units` und
#' `original_time` definiert
#'
#' -   `discrete`: ohne dem Begriff Zeitschritt, sondern immer die genau
#' Zeitstempeln geben
#'
#' ### `spat`
#'
#' Dimension `spat` ist für vector-Räumliche Daten (`points`, `lines`, und
#'                                                  `polygons`) definiert, es ist nicht ordnend aber ist es mit Varibale
#' `Spat_ID` und `Spat_Data` knüpft. Das CRS ist mit **EPSG-code**
#'   definiert.
#'
#' ### `x` und `y` in Horizontal
#'
#' Die beide Dimensionen sind für Raster (einzeln Schichten) horizontal
#' Koordinate definiert mit die Koordinaten-Werten von zentralen Punkten
#' des Zells. Das CRS ist mit **EPSG-code** definiert. Deshalb ist es nicht
#' erlaubt die Einheit oder andere zusätzliche Sachen von den beiden
#' Dimensionen zu definieren.
#'
#' ### \* `layer` und `z` in Vertikal
#'
#' Die beiden Dimensionen sind für vertikale Schichten definiert.
#'
#' -   `layer` meint eine konzeptionelle Schichten ohne genau vertikale
#' Lage
#'
#' -   `z` mit genaue vertikale (vertikale CRS noch suchen), auch möglich
#' mit Reference Fläche:
#'
#'   -   oben von Reference Fläche
#'
#' -   oder unten von Reference Fläche
#'
#' ### `vari`
#'
#' Dimension `vari` ist für mehre entsprechende Größe (Variable) definiert
#' mit `Name` und `Unit`. Die Dimension steht immer am letzten.
#'
#' ## Daten Type definieren
#'
#' ### `Time`
#'
#' Wegen den beiden kontinuierliche oder diskrete Form sind folgen beide
#' `Time-Form` definiert:
#'
#'   -   `ConTime [time]`: kontinuierliche Zeit
#'
#' -   `DisTime [time]`: diskrete Zeit
#'
#' ### `Spat`
#'
#' Wegen der Vielfalt von räumlichen Daten Form steht da unter fünf
#' `Spat-Form`:
#'
#'   -   `Vect [spat]`: Vector Daten (Punkten, Linien, Polygonen)
#'
#' -   `VectLayer [spat, layer]`: Vector Daten mit mehrere Schichten
#'
#' -   `Rast [x, y]`: Raster Daten in einzelner Schichte
#'
#' -   `RastLayer [x, y, layer]`: Raster Daten mit mehrere Schichten
#'
#' -   `RastVert [x, y, z]`: Raster Daten mit vertikale CRS
#'
#' ### `Vari`
#'
#' Wegen der unterschiedlichen Ähnlichkeit zwischen Variable zu Variable,
#' in der Sicht kann Daten auch in drei `Vari-Form` definiert:
#'
#'   -   `Vari [vari = 1] / []`: nur einzelne Variable
#'
#' -   `Array [vari]`: mehrere Variablen gleiche Struktur und
#' Dimensiongroße besetzen
#'
#' -   `Group [vari]`: mehrere Variablen gleiche Struktur und eine nicht
#' identische Dimensiongroße besetzen (meisten Zeit, besonders für
#'                                     diskrete Zeitskala), aber diese flexibel Dimension gleichen Index
#' besetzen muss
#'
#' ### Time-Spat-Vari Daten
#'
#' Zusammenfassend kann man sagen, ein komplettes TimeSpatData muss die
#' drei Makro Dimension (keine Daten Dimension) definieren. Es gibt
#' insgesamt 2 (Time) \* 5 (Spat) \* 3 (Vari) = 30 (DataForm). Aber nicht
#' alle Daten Type sind praktische, am folgen wird ein paar wichtige Daten
#' Formen gegebn:
#'
#'   -   `ConTimeVectVari`:
#'
#'   -   dim: \[time, spat, vari\] in 3D oder \[time, spat\] in 2D
#'
#' -   z.B.: Durchfluss Zeitreihe in Pegel-Netz
#'
#' -   `ConTimeVectArry`:
#'
#'   -   dim: \[time, spat, vari\] in 3D
#'
#' -   z.B.: Meteorologische Zeitreihe (Temperatur, Luftfeuchte und
#'                                      Windgeschwindigkeit) in Meteostation-Netz
#'
#' -   `ConTimeRastArry`:
#'
#'   -   dim: \[time, x, y, vari\] in 4D
#'
#' -   z.B.: reanalysieerte Meteorologische Zeitreihe (Temperatur,
#'                                                     Luftfeuchte und Windgeschwindigkeit) in Raster Form
#'
#' ## TimeSpatData in NetCDF
#'
#' Die Definition von Dimensionen von NetCDF bleibt identisch wie oben
#' Datenstruktur Dimensionen. Wegen der Begrenze von NetCDF muss man auch
#' die andere zusätzliche Dimensionen, Variablen und Attributen weiter
#' definieren. Aus der Rücksicht auf Einheitligkeit und
#' Benutzerfreundlichkeit steht es folgende Konventionen:
#'
#'   ### `Time`
#'
#'   Wegen den beiden kontinuierliche oder diskrete Form sind folgen beide
#' `Time-Form` definiert:
#'
#'   -   `ConTime [time]`: kontinuierliche Zeit
#'
#' -   `DisTime [time]`: diskrete Zeit
NULL
