import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});



/* ---------------------------------
	CSV読み込み
---------------------------------- */
app.ports.getMusicInfo.subscribe(() => {
  getCSVFile();
})

const getCSVFile = () => {
  const xhr = new XMLHttpRequest();
  xhr.onload = () => {
    const csvArray = createArray(xhr.responseText);
    const fullTime = parseFloat(csvArray[0][0]) * 1000;
    const bpm = parseFloat(csvArray[1][0]);
    const beatPerMeasure = parseFloat(csvArray[2][0]);
    const offset = parseFloat(csvArray[3][0]);
    const timePerBeat = 60 * 1000 / bpm;
    let maxCombo = 0;
    csvArray.splice(0, 4);
    const allNotes = csvArray.map((csvRow) => {
      const measure = parseFloat(csvRow[0]);
      const beat = parseFloat(csvRow[1]);
      const justTime = (measure * beatPerMeasure + beat) * timePerBeat + offset * 1000;
      csvRow.splice(0, 2);
      const notes = csvRow.map((note) => {
        if (note && !isNaN(parseInt(note, 10))) {
          const longTime = (parseInt(note, 10) === 0) ? 0 : parseFloat(note, 10) * timePerBeat;
          maxCombo = maxCombo + 1;
          if (longTime > 0) {
            maxCombo = maxCombo + Math.floor(longTime / 200);
          }
          return {justTime, longTime};
        }
        const longTime = -1;
        return {justTime, longTime};
      })
      return notes;
    })
    const tAllNotes = transpose(allNotes);
    const musicInfo = {
      fullTime,
      bpm,
      maxCombo,
      allNotes: {
        laneS: tAllNotes[0],
        laneD: tAllNotes[1],
        laneF: tAllNotes[2],
        laneJ: tAllNotes[3],
        laneK: tAllNotes[4],
        laneL: tAllNotes[5],
      }
    }
    console.log(musicInfo)
    app.ports.gotMusicInfo.send(musicInfo);
  };

  xhr.open("get", "./csv/sample_sound.csv", true);
  xhr.send(null);
}

// csvデータを配列に変換する
const createArray = (csvData) => {
  const tempArray = csvData.split("\n");
  const csvArray = new Array();
  for(let i = 0; i < tempArray.length; i++){
    csvArray[i] = tempArray[i].split(",");
  }
  return csvArray
}

// 配列を転置する
const transpose = a => a[0].map((_, c) => a.map(r => r[c]));



/* ---------------------------------
	サウンド関係
---------------------------------- */
let musicAudio;

app.ports.startMusic.subscribe(() => {
  musicAudio = new Audio();
  musicAudio.src = "./audios/sample_sound.wav";
  musicAudio.play();
  app.ports.gotCurrentMusicTime.send(0); 
})

app.ports.pauseMusic.subscribe(() => {
  musicAudio.pause();
})

app.ports.unPauseMusic.subscribe(() => {
  musicAudio.play();
  const currentMusicTime = musicAudio.currentTime * 1000;
  app.ports.gotCurrentMusicTime.send(currentMusicTime); 
})

app.ports.getCurrentMusicTime.subscribe(() => {
  const currentMusicTime = musicAudio.currentTime * 1000;
  app.ports.gotCurrentMusicTime.send(currentMusicTime);
})

app.ports.playTapSound.subscribe(() => {
  // 音発火が重い
  // const tapAudio = new Audio();
  // tapAudio.src = "./audios/tapSound2.wav";
  // tapAudio.play();
})



/* ---------------------------------
	アニメーション関係
---------------------------------- */
app.ports.playJudgeEffectAnim.subscribe(({keyStr, noteType}) => {
  const judgeEffect = document.getElementById("judgeEffect_" + keyStr);
  judgeEffect.classList.remove("long");
  if (noteType === "LONG") {
    judgeEffect.classList.add("long");
  }
  judgeEffect.classList.remove("playAnim");
  requestAnimationFrame(() => {
    judgeEffect.classList.add("playAnim");
  });
})

app.ports.playJudgeEffectTextAnim.subscribe(({keyStr, judgeText}) => {
  const judgeEffectText = document.getElementById("judgeEffectText_" + keyStr);
  judgeEffectText.textContent = judgeText;
  judgeEffectText.classList.remove("playAnim");
  requestAnimationFrame(() => {
    judgeEffectText.classList.add("playAnim");
  });
})

app.ports.playComboEffectAnim.subscribe(() => {
  const comboText = document.getElementById("comboText");
  comboText.classList.remove("playAnim");
  requestAnimationFrame(() => {
    comboText.classList.add("playAnim");
  });
})

registerServiceWorker();
