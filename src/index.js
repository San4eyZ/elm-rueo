import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var app = Elm.Main.init({
  node: document.getElementById("elm"),
  flags: {
    history: JSON.parse(localStorage.getItem("history") || "[]"),
    clientRect: {
      width: window.innerWidth,
      height: window.innerHeight
    }
  }
});

app.ports.saveHistory.subscribe(function(data) {
  const updatedHistory = JSON.stringify(data);

  localStorage.setItem("history", updatedHistory);

  app.ports.updateHistory.send(JSON.parse(updatedHistory));
});

document.body.addEventListener("keypress", e => {
  if (e.key.match(/^[a-zA-Zа-яА-ЯёЁ]$/)) {
    app.ports.searchKeyPressed.send(e.key);
  }
});

document.body.addEventListener("keydown", e => {
  if (e.key === "Backspace") {
    app.ports.backspaceKeyPressed.send(true);
  }
});

registerServiceWorker();
