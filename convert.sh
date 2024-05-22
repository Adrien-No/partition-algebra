#!/usr/bin/env sh

# converts all .dot files in .png files

# Vérifie s'il existe des fichiers correspondant au modèle
if ls img/diagram*.dot 1> /dev/null 2>&1; then
  for file in img/diagram*.dot; do
      # echo "fichier en traitement :"
      # echo "$file"
      # Extraction de la partie du nom de fichier sans extension
      base=$(basename "$file" .dot)
      # Exécution de la commande dot avec les noms de fichiers appropriés
      dot -Kneato -Tpng "$file" > "img/${base}.png"
  done
fi
# concat pngs into a single png
convert -append img/diagram*.png img/permanent/output.png
convert -append img/diagram*.png img/permanent/output2.png # bug pour ne pas avoir de flash lumineux lors de l'actualisation
make clear
