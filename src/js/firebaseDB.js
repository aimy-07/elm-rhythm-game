import firebase from 'firebase/app';
import 'firebase/database';
import {detectedError} from '../index';
import {_} from 'underscore';
const uuidv4 = require('uuid/v4');



/* ---------------------------------
	Subscriber
---------------------------------- */
export function databaseSetUpSubscriber (app) {
  // ユーザー設定の取得
  app.ports.getUserSetting.subscribe(uid => {
    firebase.database().ref(`/users/${uid}`).once('value')
      .then(
        (snapshot) => {
          const data = snapshot.val();
          const currentMusicId = data.currentMusicId ? data.currentMusicId : null;
          const currentMode = data.currentMode ? data.currentMode : null;
          const notesSpeed = data.notesSpeed ? data.notesSpeed : null;
          app.ports.gotUserSetting.send({currentMusicId, currentMode, notesSpeed});
        }
      )
      .catch(detectedError)
  });

  // 最後に選択した曲のMusicIdを保存
  app.ports.saveCurrentMusicId.subscribe(({uid, currentMusicId}) => {
    firebase.database().ref(`/users/${uid}/currentMusicId/`).set(currentMusicId, detectedError);
  });

  // 最後に選択した曲のModeを保存
  app.ports.saveCurrentMode.subscribe(({uid, currentMode}) => {
    firebase.database().ref(`/users/${uid}/currentMode/`).set(currentMode, detectedError);
  });

  // ノーツの速度を保存
  app.ports.saveNotesSpeed.subscribe(({uid, notesSpeed}) => {
    firebase.database().ref(`/users/${uid}/notesSpeed/`).set(notesSpeed, detectedError);
  });

  // 全ての楽曲の情報を取得する
  app.ports.getAllMusicInfoList.subscribe(() => {
    firebase.database().ref('/musicInfos').once('value')
      .then(
        (snapshot) => {
          const musicInfos = toArrFromObj(snapshot.val());
          const allMusicInfoList = musicInfos.map(musicInfo => {
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
              maxScore: musicInfo.maxScore
            }
          });
          console.log("allMusicInfoList", allMusicInfoList)
          app.ports.gotAllMusicInfoList.send(allMusicInfoList);
        }
      )
      .catch(detectedError)
  })

  // 過去の自分のプレイデータの取得
  app.ports.getOwnRecords.subscribe(uid => {
    firebase.database().ref(`/users/${uid}/playRecords`).once('value')
      .then(
        (snapshot) => {
          if (snapshot.val()) {
            const records = toArrFromObj(snapshot.val());
            app.ports.gotOwnRecords.send(records);
          } else {
            app.ports.gotOwnRecords.send([]);
          }
        }
      )
      .catch(detectedError)
  });

  // ランキングデータの取得
  // TODO: firebaseルールでuserNameへのアクセスを許可
  app.ports.getPublicRecords.subscribe(() => {
    firebase.database().ref(`/publicRecords`).once('value')
      .then(
        (snapshot) => {
          const publicRecords = toArrFromObj(snapshot.val());
          const getPublicRecords = publicRecords.map(publicRecord => {
            const getBestScores =
              publicRecord.bestScores
                ? publicRecord.bestScores.map(async record => {
                  const getUserName = await firebase.database().ref(`/users/${record.uid}/userName`).once('value');
                  return {userName: getUserName.val(), score: record.score}
                })
                : [];
            return Promise.all(getBestScores)
              .then((bestScores) => {
                return {
                  csvFileName: publicRecord.csvFileName,
                  bestScores: bestScores
                }
              })
              .catch(detectedError)
          });
          Promise.all(getPublicRecords)
            .then((publicRecords) => {
              console.log('publicRecords', publicRecords);
              app.ports.gotPublicRecords.send(publicRecords);
            })
            .catch(detectedError)
        }
      )
      .catch(detectedError)
  });

  // プレイリザルトの保存
  app.ports.saveRecord.subscribe(({uid, csvFileName, combo, score}) => {
    const recordId = uuidv4();
    const createdAt = Date.now();
    const saveRecord = firebase.database().ref(`/records/${csvFileName}/${uid}/${recordId}/`).set(
      {uid, csvFileName, combo, score, createdAt},
      detectedError
    );

    let isHighScore;
    const updateOwnPlayRecord = firebase.database().ref(`/users/${uid}/playRecords/${csvFileName}`).transaction(
      (playRecord) => {
        if (playRecord) {
          isHighScore = score > playRecord.bestScore;
          return {
            csvFileName,
            bestCombo: combo > playRecord.bestCombo ? combo : playRecord.bestCombo,
            bestScore: score > playRecord.bestScore ? score : playRecord.bestScore,
            playCount: playRecord.playCount + 1
          }
        } else {
          isHighScore = true;
          return {
            csvFileName,
            bestCombo: combo,
            bestScore: score,
            playCount: 1
          }
        }
      },
      detectedError
    );

    const updatePublicPlayRecord = firebase.database().ref(`/publicRecords/${csvFileName}/bestScores`).transaction(
      (bestScores) => {
        if (score == 0) {
          return bestScores ? bestScores : [];
        }
        if (bestScores) {
          const newBestScores = _.sortBy(bestScores.concat({uid, score}), (record => record.score)).reverse();
          if (newBestScores.length > 3) {
            return newBestScores.slice(0, 3);
          } else {
            return newBestScores
          }
        } else {
          return [{uid, score}]
        }
      },
      detectedError
    );

    Promise.all([saveRecord, updateOwnPlayRecord, updatePublicPlayRecord])
      .then(() => {
        app.ports.savedRecord.send(isHighScore);
      })
      .catch(detectedError)
  });
}



/* ---------------------------------
	連想配列を配列に変換する関数
---------------------------------- */
export const toArrFromObj = obj => {
  const arr = [];
  Object.keys(obj).forEach((key) => {
    arr.push(obj[key]);
  });
  return arr;
}