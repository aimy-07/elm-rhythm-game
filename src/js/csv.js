import axios from 'axios'
import firebase from 'firebase/app';
import 'firebase/storage';



/* ---------------------------------
	定数
---------------------------------- */
const perfectScore = 2000;
const longScore = 100;
const longTimeOffset = 150;
const longTimeDuration = 200;


const csvFileNameList = [
  "sample_sound-normal",
  "sample_sound-hard",
  "sample_sound-master",
  "sample_sound1-normal",
  "sample_sound1-hard",
  "sample_sound1-master",
  "sample_sound2-normal",
  "sample_sound2-hard",
  "sample_sound2-master",
]



/* ---------------------------------
	Subscriber
---------------------------------- */
export function csvSetUpSubscriber (app) {
  // 全ての楽曲の情報をCSVから取得する
  app.ports.getAllMusicInfoList.subscribe(() => {
    const promises = csvFileNameList.map((csvFileName) => {
      const csvFileRef = firebase.storage().ref('csv/' + csvFileName + '.csv');
      return new Promise((resolve) => {
        csvFileRef.getDownloadURL().then(url => {
          return axios.get(url)
        })
        .then(response => {
          const csvArray = createArray(response.data);
          resolve(getMusicInfo(csvFileName, csvArray).musicInfo);
        })
        .catch(error => {
          console.log(error);
        });
      })
    })
    Promise.all(promises).then((allMusicInfoList) => {
      app.ports.gotAllMusicInfoList.send(allMusicInfoList);
    });
  })

  // プレイ楽曲の楽曲情報をCSVから取得する
  app.ports.getCurrentMusicInfo.subscribe((csvFileName) => {
    if (!csvFileNameList.includes(csvFileName)) {
      console.error("invalid csvFileName.");
      location.href = "/";
      return;
    }
    const csvFileRef = firebase.storage().ref('csv/' + csvFileName + '.csv');
    csvFileRef.getDownloadURL().then(url => {
      return axios.get(url)
    })
    .then(response => {
      const csvArray = createArray(response.data);
      const {musicInfo, allNotes} = getMusicInfo(csvFileName, csvArray);
      app.ports.gotCurrentMusicInfo.send({
        musicInfoDto: musicInfo,
        noteDtos: allNotes,
      });
    })
    .catch(error => {
      console.log(error);
    });
  })
}



// 楽曲の楽曲情報をCSVから取得する
const getMusicInfo = (csvFileName, csvArray) => {
  const musicName = csvArray[0][0];
  const composer = csvArray[1][0];
  const mode = csvArray[2][0];
  const level = parseInt(csvArray[3][0], 10);
  const fullTime = parseFloat(csvArray[4][0]) * 1000;
  const bpm = parseFloat(csvArray[5][0]);
  const beatPerMeasure = parseFloat(csvArray[6][0]);
  const offset = parseFloat(csvArray[7][0]);
  csvArray.splice(0, 8);

  const timePerBeat = 60 * 1000 / bpm;
  let maxCombo = 0;
  let maxScore = 0;
  const allNotes = [];

  csvArray.forEach((csvRow) => {
    const measure = parseFloat(csvRow[0]);
    const beat = parseFloat(csvRow[1]);
    const justTime = (measure * beatPerMeasure + beat) * timePerBeat + offset * 1000;

    csvRow.splice(0, 2);
    csvRow.forEach((note, index) => {
      const keyStr = createKeyStr(index);
      let longTime = -1;
      if (note && !isNaN(parseInt(note, 10)) && keyStr != "") {
        longTime = (parseInt(note, 10) === 0) ? 0 : parseFloat(note) * timePerBeat;
        maxCombo += 1;
        maxScore += perfectScore;
        if (longTime > 0) {
          const longCount = Math.floor ((longTime - longTimeOffset) / longTimeDuration) + 1
          if (longCount > 0) {
            maxCombo += 1 * longCount;
            maxScore += longScore * longCount;
          }
        }
      }
      allNotes.push({keyStr, justTime, longTime})
    })
  });

  const musicInfo = {
    csvFileName,
    musicName,
    composer,
    mode,
    level,
    fullTime,
    bpm,
    maxCombo,
    maxScore
  }
  console.log("musicInfo", musicInfo);
  console.log("allNotes", allNotes);
  return {musicInfo, allNotes};
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

// csvの列番号をキーに変換する
const createKeyStr = (csvNum) => {
  switch(csvNum) {
    case 0:
      return "S";
    case 1:
      return "D";
    case 2:
      return "F";
    case 3:
      return "J";
    case 4:
      return "K";
    case 5:
      return "L";
    default:
      return "";
  }
}