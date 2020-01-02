import firebase from 'firebase/app';
import 'firebase/database';
import {detectedError} from '../index';
import {_} from 'underscore';
const uuidv4 = require('uuid/v4');



/* ---------------------------------
	Subscriber
---------------------------------- */
export function firebaseDBSetUpSubscriber (app) {
  // ユーザー設定の取得
  app.ports.getUserSetting.subscribe(uid => {
    firebase.database().ref(`/userDatas/${uid}`).once('value')
      .then(
        (snapshot) => {
          if (!snapshot.val()) {
            app.ports.gotUserSetting.send({
              currentMusicId: null,
              currentMode: null,
              notesSpeed: null,
              bgmVolume: null,
              seVolume: null
            });
            return;
          }
          const data = snapshot.val();
          const currentMusicId = data.currentMusicId ? data.currentMusicId : null;
          const currentMode = data.currentMode ? data.currentMode : null;
          const notesSpeed = data.notesSpeed ? data.notesSpeed : null;
          const bgmVolume = data.notesSpeed ? data.bgmVolume : null;
          const seVolume = data.notesSpeed ? data.seVolume : null;
          app.ports.gotUserSetting.send({currentMusicId, currentMode, notesSpeed, bgmVolume, seVolume});
        }
      )
      .catch(detectedError)
  });

  // 最後に選択した曲のMusicIdを保存
  app.ports.saveCurrentMusicId.subscribe(({uid, currentMusicId}) => {
    firebase.database().ref(`/userDatas/${uid}/currentMusicId/`).set(currentMusicId, detectedError);
  });

  // 最後に選択した曲のModeを保存
  app.ports.saveCurrentMode.subscribe(({uid, currentMode}) => {
    firebase.database().ref(`/userDatas/${uid}/currentMode/`).set(currentMode, detectedError);
  });

  // ノーツの速度を保存
  app.ports.saveNotesSpeed.subscribe(({uid, notesSpeed}) => {
    firebase.database().ref(`/userDatas/${uid}/notesSpeed/`).set(notesSpeed, detectedError);
  });

  // BGGM音量を保存
  app.ports.saveBgmVolume.subscribe(({uid, bgmVolume}) => {
    firebase.database().ref(`/userDatas/${uid}/bgmVolume/`).set(bgmVolume, detectedError);
  });

  // SE音量を保存
  app.ports.saveSeVolume.subscribe(({uid, seVolume}) => {
    firebase.database().ref(`/userDatas/${uid}/seVolume/`).set(seVolume, detectedError);
  });

  // 過去の自分のプレイデータの取得
  app.ports.getOwnRecords.subscribe(uid => {
    firebase.database().ref(`/userDatas/${uid}/playRecords`).once('value')
      .then(
        (snapshot) => {
          if (!snapshot.val()) {
            app.ports.gotOwnRecords.send([]);
            return;
          }
          const records = toArrFromObj(snapshot.val());
          app.ports.gotOwnRecords.send(records);
        }
      )
      .catch(detectedError)
  });

  // ランキングデータの取得
  app.ports.getPublicRecords.subscribe(() => {
    firebase.database().ref(`/publicRecords`).once('value')
      .then(
        (snapshot) => {
          if (!snapshot.val()) {
            app.ports.gotPublicRecords.send([]);
            return;
          }
          const publicRecords = toArrFromObj(snapshot.val());
          const getPublicRecords = publicRecords.map(publicRecord => {
            const getBestScores =
              publicRecord.bestScores
                ? publicRecord.bestScores.map(async record => {
                  const getUserName = await firebase.database().ref(`/users/${record.uid}/userName`).once('value');
                  return {uid: record.uid, userName: getUserName.val(), score: record.score}
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
    const saveRecord = () => firebase.database().ref(`/records/${csvFileName}/${uid}/${recordId}/`).set(
      {uid, csvFileName, combo, score, createdAt},
      detectedError
    );

    let isHighScore;
    const updateOwnPlayRecord = () => firebase.database().ref(`/userDatas/${uid}/playRecords/${csvFileName}`).transaction(
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
          isHighScore = score === 0 ? false : true;
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

    const updatePublicPlayRecord = () => firebase.database().ref(`/publicRecords/${csvFileName}/`).transaction(
      (publicRecord) => {
        if (!publicRecord) {
          return {csvFileName, bestScores: [{uid, score}]}
        }
        const bestScores = publicRecord.bestScores;
        if (score == 0) {
          return bestScores ? {csvFileName, bestScores} : {csvFileName, bestScores: []};
        }
        const newBestScores = _.sortBy(bestScores.concat({uid, score}), (record => record.score)).reverse();
        if (newBestScores.length > 3) {
          return {csvFileName, bestScores: newBestScores.slice(0, 3)}
        } else {
          return {csvFileName, bestScores: newBestScores}
        }
      },
      detectedError
    );

    saveRecord()
      .then(updateOwnPlayRecord)
      .then(updatePublicPlayRecord)
      .then(() => {
        app.ports.savedRecord.send(isHighScore);
      })
      .catch(detectedError)
  });
}



/* ---------------------------------
	連想配列を配列に変換する関数
---------------------------------- */
const toArrFromObj = obj => {
  const arr = [];
  Object.keys(obj).forEach((key) => {
    arr.push(obj[key]);
  });
  return arr;
}