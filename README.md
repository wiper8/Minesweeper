
## Obtention de résultats

1. Aller dans `main.R` et exécuter les fonctions désirées. Les résultats des fonctions `get_*` sont automatiquement
enregistrés progressivement dans le dossier `data`. On peut interrompre les exécutions trop lentes et les résultats
partiels sont tout de même conservés.

2. Aller dans `analyse.qmd` et render le document. Ou de manière équivalente, exécuter au terminal
```
quarto preview analyse.qmd
```
Tous les résultats du dossier `data` seront mis dans l'analyse automatiquement.


## Exécuter les tests unitaires

```
source("tests/testthat.R")
```

## Idées d'améliorations futures (TODOs)

- La fonction `choose()` pourrait-elle _overflow_? Approximation sinon ?

- Moins fréquent qu'auparavant, mais il reste des sélections qui demeurent lentes. Trouver où et pourquoi.
Remettre le `if (temps > 60) browser()` pour trouver les pires cas.

- Dans `probabilistic_clicker`, serait-ce possible davoir un shortcut de ne pas savoir la probabilité, mais de savoir
que cest la minimale dans un cluster? Puis minimale entre tous les clusters?

- Faire les autres TODOS variés qui demeurent dans le code.

- Faire un `human_clicker` : ne sait pas résoudre les clusters avec > 6(?) combinaisons, estime prob void avec les mines
initialement dans la grille (`nombre de mines init / prod(dims)`), ou d'autres idées de limites plus humaines, pour que
le clicker soit entre le `certain_else_random`, et le `smart_clicker`.
