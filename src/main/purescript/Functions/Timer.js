"use strict";

var handle = null;
var timeoutTime = 0;

function setTimer(time) {
    return setTimeout(ev => {
        document.getElementById("lockButton").dispatchEvent(new MouseEvent("click", {
            bubbles: true,
            cancelable: true,
            view: window,
        }))
    }, time * 60000)
}

function resetTimer() { 
	if (handle != null && timeoutTime > 0) {
		clearTimeout(handle);
		handle = setTimer(timeoutTime);
	}
	return;
}

const activateTimer = function(time) {
    return function() {
        timeoutTime = time;
        handle = setTimer(time)
        return;
    }
}

const stopTimer = function() {
    clearTimeout(handle);
	timeoutTime = 0;
	handle = null;
    return;
}

export {
    activateTimer,
	resetTimer,
    stopTimer
}
