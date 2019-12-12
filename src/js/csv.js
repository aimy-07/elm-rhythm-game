import axios from 'axios'
import firebase from 'firebase/app';
import 'firebase/storage';



/* ---------------------------------
	定数
---------------------------------- */
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
  // 全ての楽曲の情報をFirebaseから取得する
  app.ports.getAllMusicInfoList.subscribe(() => {
    const allMusicInfoList = [];
    firebase.database().ref('/music_infos').once('value').then((snapshot) => {
      const datas = snapshot.val();
      Object.keys(datas).forEach((key) => {
        const musicInfo = datas[key];
        allMusicInfoList.push({
          csvFileName: musicInfo.csv_file_name,
          musicName: musicInfo.music_name,
          composer: musicInfo.composer,
          mode: musicInfo.mode,
          level: musicInfo.level,
          fullTime: musicInfo.full_time,
          bpm: musicInfo.bpm,
          maxCombo: musicInfo.max_combo,
          maxScore: musicInfo.max_score
        });
      });
      console.log('allMusicInfoList', allMusicInfoList);
      app.ports.gotAllMusicInfoList.send(allMusicInfoList);
    });
  })

  // プレイ楽曲の楽曲情報をFirebaseとCSVから取得する
  app.ports.getCurrentMusicInfo.subscribe((csvFileName) => {
    if (!csvFileNameList.includes(csvFileName)) {
      console.error("invalid csvFileName.");
      location.href = "/";
      return;
    }

    firebase.database().ref(`/music_infos/${csvFileName}`).once('value').then((snapshot) => {
      const musicInfo = snapshot.val();
      const csvFileRef = firebase.storage().ref(`csv/${csvFileName}.csv`);
      csvFileRef.getDownloadURL().then(url => {
        return axios.get(url)
      })
      .then(response => {
        const csvArray = createArray(response.data);
        const musicInfoDto = {
          csvFileName: musicInfo.csv_file_name,
          musicName: musicInfo.music_name,
          composer: musicInfo.composer,
          mode: musicInfo.mode,
          level: musicInfo.level,
          fullTime: musicInfo.full_time,
          bpm: musicInfo.bpm,
          maxCombo: musicInfo.max_combo,
          maxScore: musicInfo.max_score
        }
        const noteDtos = getNoteDtos(musicInfo.bpm, musicInfo.beats_count_per_measure, musicInfo.offset, csvArray);
        app.ports.gotCurrentMusicInfo.send({musicInfoDto, noteDtos});
      })
      .catch(error => {
        console.log(error);
      });
    });
  })
}



// 楽曲の譜面情報をCSVから取得する
const getNoteDtos = (bpm, beatsCountPerMeasure, offset, csvArray) => {
  const timePerBeat = 60 * 1000 / bpm;
  const noteDtos = [];

  csvArray.forEach((csvRow) => {
    const measure = parseFloat(csvRow[0]);
    const beat = parseFloat(csvRow[1]);
    const justTime = (measure * beatsCountPerMeasure + beat) * timePerBeat + offset * 1000;
    csvRow.splice(0, 2);
    csvRow.forEach((note, index) => {
      const keyStr = createKeyStr(index);
      let longTime = -1;
      if (note && !isNaN(parseInt(note, 10)) && keyStr != "") {
        longTime = (parseInt(note, 10) === 0) ? 0 : parseFloat(note) * timePerBeat;
      }
      noteDtos.push({keyStr, justTime, longTime})
    })
  });

  return noteDtos;
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