import axios from 'axios'
import firebase from 'firebase/app';
import 'firebase/storage';
import {toArrFromObj} from '../index';


/* ---------------------------------
	Subscriber
---------------------------------- */
export function csvSetUpSubscriber (app) {
  // 全ての楽曲の情報をFirebaseから取得する
  app.ports.getAllMusicInfoList.subscribe(() => {
    firebase.database().ref('/musicInfos').once('value').then(
      (snapshot) => {
        const musicInfos = toArrFromObj(snapshot.val());
        const getAllMusicInfoList = musicInfos.map(musicInfo => {
          const getBestRecords =
            musicInfo.bestRecords
              ? musicInfo.bestRecords.map(async record => {
                const getUserName = await firebase.database().ref(`/users/${record.uid}/userName`).once('value');
                return {userName: getUserName.val(), bestScore: record.bestScore}
              })
              : [];
          return Promise.all(getBestRecords)
            .then((bestRecords) => {
              return {
                musicId: musicInfo.musicId,
                csvFileName: musicInfo.csvFileName,
                musicName: musicInfo.musicName,
                composer: musicInfo.composer,
                mode: musicInfo.mode,
                level: musicInfo.level,
                fullTime: musicInfo.fullTime,
                bpm: musicInfo.bpm,
                beatsCountPerMeasure: musicInfo.beatsCountPerMeasure,
                offset: musicInfo.offset,
                maxCombo: musicInfo.maxCombo,
                maxScore: musicInfo.maxScore,
                bestRecords: bestRecords
              }
            })
            .catch((err) => {
              console.error(err);
              // TODO: ネットワークエラー画面に飛ばす
            })
        });
        Promise.all(getAllMusicInfoList)
          .then((allMusicInfoList) => {
            console.log('allMusicInfoList', allMusicInfoList);
            app.ports.gotAllMusicInfoList.send(allMusicInfoList);
          })
          .catch((err) => {
            console.error(err);
            // TODO: ネットワークエラー画面に飛ばす
          })
      },
      (err) => {
        console.error(err);
        // TODO: ネットワークエラー画面に飛ばす
      }
    );
  })

  // プレイ楽曲の楽曲情報をCSVから取得する
  app.ports.getAllNotes.subscribe(({csvFileName, bpm, beatsCountPerMeasure, offset}) => {
    const csvFileRef = firebase.storage().ref(`csv/${csvFileName}.csv`);
    csvFileRef.getDownloadURL().then(url => {
      return axios.get(url);
    })
    .then(response => {
      const csvArray = createArray(response.data);
      const noteDtos = getNoteDtos(bpm, beatsCountPerMeasure, offset, csvArray);
      app.ports.gotAllNotes.send(noteDtos);
    })
    .catch(error => {
      console.error(error);
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