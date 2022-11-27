/*
 * Load audio with XMLHttpRequest similar to other resources
 * Expect rsponseType of arraybuffer
 *
 * Decode using AudioContext
 */

function setClickEventById(id, fn, options = {}) {
	let el = document.getElementById(id)
	if(el) {
		el.addEventListener("click", fn, options);
		console.log(`Attached event for ${id}`);
	}
}

const noteMap = new Map();
noteMap.set("A", 27.50);
noteMap.set("A#", 29.14);
noteMap.set("Bb", 29.14);
noteMap.set("B", 30.87);
noteMap.set("C", 16.35);
noteMap.set("C#", 17.32);
noteMap.set("Db", 17.32);
noteMap.set("D", 18.35);
noteMap.set("D#", 19.45);
noteMap.set("Eb", 19.45);
noteMap.set("E", 20.60);
noteMap.set("F", 21.83);
noteMap.set("F#", 23.12);
noteMap.set("Gb", 23.12);
noteMap.set("G", 24.50);
noteMap.set("G#", 25.96);
noteMap.set("Ab", 25.96);

function frequencyFor(note, octave) {
	let nVal = noteMap.get(note) || 27.50;
	let fVal = octave < 0 ? nVal : nVal * octave;
	return fVal;
}

function startSine(ctx, note) {
	return (e) => {
		const oscillator = new OscillatorNode(ctx, {
			type: "square",
			frequency: frequencyFor(note, 1)
		});
		const gain = new GainNode(ctx);
		//gain.gain.setValueAtTime(0, ctx.currentTime);
		//gain.gain.exponentialRampToValueAtTime(0.9, ctx.currentTime + 0.01);
		//gain.gain.exponentialRampToValueAtTime(0.9, ctx.currentTime + 0.17);
		//gain.gain.exponentialRampToValueAtTime(0.01, ctx.currentTime + 0.190);
		oscillator.connect(gain);
		gain.connect(ctx.destination);
		oscillator.start(ctx.currentTime);
		oscillator.stop(ctx.currentTime + 0.2);
		//setClickEventById(`playA`, startSine(ctx, note), { once: true });
	}
}

function attachEvents(ctx) {
	//setClickEventById(`playA`, startSine(ctx, "A"), { once: true });
	for (let k of noteMap.keys()) {
		setClickEventById(`play${k}`, startSine(ctx, k));
	}
}

function initializeContext() {
	const audioCtx = new window.AudioContext();
	return audioCtx;
}

document.addEventListener("load", setClickEventById("initialize", () => { attachEvents(initializeContext()); }));
