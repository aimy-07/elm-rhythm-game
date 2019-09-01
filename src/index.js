import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.addJudgeEffect.subscribe(({styleLeft, judgeText, noteType}) => {
  // エフェクト
  const div1 = document.createElement('div');
  div1.className = noteType === "SINGLE" ? 'play_noteEffect' : 'play_noteEffect long';
  div1.style.left = styleLeft;
  // 判定文字
  const div2 = document.createElement('div');
  div2.className = 'judge_effectText';
  div2.style.left = styleLeft;
  div2.textContent = judgeText;
  document.getElementById('judge_area').appendChild(div2);
  if (judgeText !== "Miss") { 
    document.getElementById('judge_area').appendChild(div1);
  }
})

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

  xhr.open("get", "./csv/sample.csv", true);
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

let audioElem;

app.ports.startMusic.subscribe(() => {
  audioElem = new Audio();
  audioElem.src = "./audios/sample_sound.wav";
  audioElem.play();
})

app.ports.pauseMusic.subscribe(() => {
  audioElem.pause();
})

app.ports.unPauseMusic.subscribe(() => {
  audioElem.play();
})


registerServiceWorker();
