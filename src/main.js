import { Elm } from './Main.elm'

import './CustomDialog.js'


const app =
    Elm.Main.init({
        node: document.getElementById('app'),
    });

app.ports.setFavorite.subscribe(function (word) {
    var favorites = JSON.parse(localStorage.getItem("favorites")) || [];
    if (!favorites.includes(word)) {
        favorites.push(word);
        localStorage.setItem("favorites", JSON.stringify(favorites));
        app.ports.receiveFavorites.send(favorites);
    }
});

app.ports.removeFavorite.subscribe(function(word) {
    var favorites = JSON.parse(localStorage.getItem("favorites")) || [];
    favorites = favorites.filter(function(favorite) {
        return favorite !== word;
    });
    localStorage.setItem("favorites", JSON.stringify(favorites));
    app.ports.receiveFavorites.send(favorites);
});

app.ports.getFavorites.subscribe(function () {
    var favorites = JSON.parse(localStorage.getItem("favorites")) || [];
    app.ports.receiveFavorites.send(favorites);
});

app.ports.sendTTS.subscribe(function(word) {
    const utterance = new SpeechSynthesisUtterance(word);
    const voices = speechSynthesis.getVoices();
    utterance.voice = voices[3];

    speechSynthesis.speak(utterance);
});

app.ports.toggleDialog.subscribe(() => {
    let dialog = document.querySelector('custom-dialog');

    if (!dialog) {
        dialog = document.createElement('custom-dialog');
        document.body.appendChild(dialog);
    }
    if (dialog.dialog && dialog.dialog.open) {
        dialog.closeDialog();
    } else {
        dialog.openDialog();
    }

    console.log(dialog);
});
