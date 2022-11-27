function anyButtonsPressed() {
  const pads = navigator.getGamepads();
  for (let pad of pads) {
    for (let btn of pad.buttons) {
      if (btn.pressed) {
        return true;
      }
    }
  }
  return false;
}

function manageGamePads(app) {
  var buttonPressed = false;

  window.addEventListener("gamepadconnected", (e) => {
    app.ports.gamePadConnected.send(true);
    console.log("Gamepad connected");
  });

  window.addEventListener("gamepaddisconnected", (e) => {
    app.ports.gamePadConnected.send(false);
    console.log("Gamepad connected");
  });

  app.ports.checkButtons.subscribe((msg) => {
    const anyPressed = anyButtonsPressed();
    const changed = anyPressed != buttonPressed;
    if (changed) {
      console.log(changed);
      if (buttonPressed) {
        app.ports.padButtonsDown.send(false);
      } else {
        app.ports.padButtonsDown.send(true);
      }
      buttonPressed = anyPressed;
    }
  });
}
