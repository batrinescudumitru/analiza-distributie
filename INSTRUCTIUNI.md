# Instrucțiuni GitHub Pages — Analiza Repartiției

## Structura fișierelor create

```
stat_app/
├── app.R        ← aplicația Shiny (pentru rulare locală în R)
└── index.html   ← versiunea web cu WebR/Shinylive (pentru GitHub Pages)
```

---

## Pași pentru publicarea pe GitHub Pages

### 1. Creează un cont GitHub
- Mergi la https://github.com și înregistrează-te (gratuit)

### 2. Creează un repository nou
- Click pe **"New"** (buton verde)
- Nume repository: de ex. `analiza-distributie`
- Lasă **Public** bifat
- Click **"Create repository"**

### 3. Încarcă fișierele
- Click pe **"uploading an existing file"**
- Trage fișierele `app.R` și `index.html` în fereastra browser-ului
- Click **"Commit changes"**

### 4. Activează GitHub Pages
- Mergi la **Settings** (tab-ul din repository)
- Scroll jos până la secțiunea **"Pages"**
- La **Source**, selectează: **Deploy from a branch**
- La **Branch**, selectează: **main** → **/ (root)**
- Click **Save**

### 5. Accesează aplicația
- După ~2 minute, aplicația va fi disponibilă la:
  `https://UTILIZATORUL_TAU.github.io/analiza-distributie/`

---

## Cum funcționează aplicația

1. **Încarcă un fișier CSV** (cu date numerice)
2. **Selectează coloana** de analizat
3. **Apasă "Analizează distribuția"**
4. Vei vedea:
   - Histogramă + curba densității
   - Q-Q Plot pentru testarea normalității
   - Statistici descriptive + teste Shapiro-Wilk și KS
   - Distribuțiile sugerate
5. **Apasă "Interpretează cu ChatGPT"** → se deschide ChatGPT cu toate rezultatele pre-completate

---

## Format CSV acceptat

```csv
varsta,greutate,inaltime
25,70,175
30,65,168
...
```

Separatorul poate fi virgulă (`,`) sau punct-virgulă (`;`).

---

## Rulare locală (în RStudio)

```r
# Instalează pachetele necesare
install.packages(c("shiny", "ggplot2", "moments"))

# Rulează aplicația
shiny::runApp("app.R")
```
