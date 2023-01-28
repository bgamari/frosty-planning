function onSailorClicked(ev) {
    for (const el of document.querySelectorAll(".sailor")) {
        el.classList.remove("selected");
    }

    const sailor = ev.target.dataset.sailor;
    for (const el of document.querySelectorAll(`.sailor[data-sailor="${sailor}"]`)) {
        el.classList.add("selected");
    }
}

window.onload = function() {
    for (const el of document.querySelectorAll(".sailor")) {
        el.onclick = onSailorClicked;
    };
};

