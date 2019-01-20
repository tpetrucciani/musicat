This is a web UI to display a catalogue of music albums, possibly with links to streaming services (currently Spotify and Qobuz are supported).

See an example [here](https://tpetrucciani.github.io/musicat)
(using [this](https://tpetrucciani.github.io/musicat/data/catalogue.json) catalogue file).

The catalogue is stored as a JSON file and covers for each album are stored in a folder. For now, these must be added/edited by hand; I'll then work on a script to make this faster. The files should be organized as

    _site/
      data/
        catalogue
        covers/
          (album covers, possibly organized in subdirectories)
      resources/
        (font files for Source Sans Pro)
      index.html
      main.js
      style.css

where `catalogue` is the JSON file and where `main.js` and `style.css` are compiled using Elm and Sass. Then, serving the `_site` directory, `index.html` displays the albums in the catalogue. You can select which genre to see. For each genre, albums are organized by artist; you can filter the artists to show with a search field. Each album can have one or more "sources" (available as a local file, in Spotify, in Qobuz, or nowhere) and you can select which albums to show by source; albums can also be marked as archived or not. Albums with Spotify or Qobuz sources include links that open the applications (using the `spotify:` and `qobuzapp:` URI schemes).

# Main goals for the future

- Improve the layout and UI
- Improve rendering performance (probably using `Html.Lazy` and `Html.Keyed` in Elm)
- ~~Allow to group albums by sub-genre within each artist (e.g. for classical music we could have albums filed under "Schubert / Orchestral" and "Schubert / Song")~~
- Add the possibility to "star" and "unstar" albums directly from the web page
- ~~Add album booklets in the catalogue~~
- Build a script to help in organizing the catalogue and downloading covers
