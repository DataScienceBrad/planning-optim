Pour faire tourner l'appli shiny sous windows:


1) Etape à effectuer uniquement la première fois
Installer R, en cliquant sur le gros lien 'Download R 3.4.3 for Windows' sur cette page https://cran.r-project.org/bin/windows/base/
Suivre les étapes d'installation classiques sous windows

2) Lancer R. Une fenetre s'ouvre avec un terminal en ligne de commande.

3) Etape à effectuer uniquement la première fois (installer les librairies utilisées par l'application)
Taper dans le terminal:

install.packages('shiny')

Une fenêtre de dialogue vous propose éventuellement de créer un librairie personnelle pour des raisons de droits d'écriture, vous acceptez.
On vous demande de choisir un serveur CRAN pour la session dans une fenetre de dialogue. C'est le serveur depuis lequel vous allez telecharger les paquets. Prenez celui que vous voulez, pas trop loin quand meme.
Normalement, des lignes défilent sur le terminal et tout se telecharge bien.
Puis télécharger deux autres packages:

install.packages('data.table')
install.packages('lpSolve')

4) Se placer dans le dossier ShinyAppPackaged, présent dans le dossier archivé avec ce mail. Pour cela ouvrir le menu:
file > change dir...
Aller dans le dossier avec le code de l'application çi joint, puis sélectionner ShinyAppPackaged

5) Taper dans le terminal:

library(shiny)

6) Taper dans le terminal:

runApp()

Cela devrait lancer l'application dans le navigateur par defaut, qui tourne désormais en local.
