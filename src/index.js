import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.addJudgeEffect.subscribe(({styleLeft, judgeText}) => {
  const div = document.createElement('div');
  div.className = 'judge_effect';
  div.style.left = styleLeft;
  div.textContent = judgeText;
  document.getElementById('judge_area').appendChild(div);
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
    const beatCount = parseFloat(csvArray[2][0]);
    const offset = parseFloat(csvArray[3][0]);
    const timePerBpm = 60 * 1000 / bpm;
    csvArray.splice(0, 4);
    const allNotes = csvArray.map((concurrentNote) => {
      const justTime = (parseFloat(concurrentNote[0]) * beatCount + parseFloat(concurrentNote[1])) * timePerBpm + offset * 1000;
      concurrentNote.splice(0, 2);
      const notes = concurrentNote.map((note) => {
        if (note && !isNaN(parseInt(note, 10))) {
          return (parseInt(note, 10) === 0) ? 0 : parseFloat(note, 10) * timePerBpm
        }
        return -1;
      })
      return {justTime, notes}
    })
    const musicInfo = {fullTime, bpm, allNotes}
    console.log(musicInfo)
    app.ports.gotMusicInfo.send(musicInfo);
  };

  xhr.open("get", "./csv/sample.csv", true);
  xhr.send(null);
}

const createArray = (csvData) => {
  const tempArray = csvData.split("\n");
  const csvArray = new Array();
  for(let i = 0; i < tempArray.length; i++){
    csvArray[i] = tempArray[i].split(",");
  }
  return csvArray
}

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
