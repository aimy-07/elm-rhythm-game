import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import {csvSetUpSubscriber} from './js/csv'
import {audioSetUpSubscriber} from './js/audio'
import {animationSetUpSubscriber} from './js/animation'

import firebase from 'firebase/app';
import 'firebase/auth';
import 'firebase/database';
import {firebaseConfig} from './config';



/* ---------------------------------
  Firebase init
---------------------------------- */
firebase.initializeApp(firebaseConfig);

const googleAuthProvider = new firebase.auth.GoogleAuthProvider();



/* ---------------------------------
  Elm init
---------------------------------- */
const app = Elm.Main.init({
  node: document.getElementById('root')
});

registerServiceWorker();



/* ---------------------------------
  Firebase Auth
---------------------------------- */
firebase.auth().onAuthStateChanged((user) => {
  if (!user) {
    // サインインしていない状態
    app.ports.onAuthStateChanged.send(null);
  } else {
    // サインイン済み
    app.ports.onAuthStateChanged.send({
      uid: user.uid,
      userName: user.displayName,
      pictureUrl: user.photoURL,
    });
  }
});



/* ---------------------------------
  Subscriber
---------------------------------- */
app.ports.signIn.subscribe(_ => {
  firebase.auth().signInWithPopup(googleAuthProvider)
    .then((_) => {})
    .catch((error) => {});
});

app.ports.signOut.subscribe(_ => {
  firebase.auth().signOut()
    .then((_) => {})
    .catch((error) => {});
});

csvSetUpSubscriber(app);
audioSetUpSubscriber(app);
animationSetUpSubscriber(app);


/* ---------------------------------
  プレイリザルトの保存
---------------------------------- */
const uuidv4 = require('uuid/v4');

app.ports.saveRecord.subscribe(({uid, csvFileName, combo, score}) => {
  const recordId = uuidv4();
  const createdAt = Date.now();
  const saveRecord = firebase.database().ref(`/records/${csvFileName}/${uid}/${recordId}/`).set({
      uid: uid,
      csv_file_name: csvFileName,
      combo: combo,
      score: score,
      created_at: createdAt
    }
  );
  let isHighScore;
  const updateOwnPlayRecord = firebase.database().ref(`/users/${uid}/play_records/${csvFileName}`).transaction((playRecord) => {
    if (playRecord) {
      isHighScore = score > playRecord.best_score;
      return {
        csv_file_name: csvFileName,
        best_combo: combo > playRecord.best_score ? combo : playRecord.best_score,
        best_score: score > playRecord.best_score ? score : playRecord.best_score,
        play_count: playRecord.play_count + 1
      }
    } else {
      isHighScore = true;
      return {
        csv_file_name: csvFileName,
        best_combo: combo,
        best_score: score,
        play_count: 1
      }
    }
  });
  Promise.all( [saveRecord, updateOwnPlayRecord] )
    .then(() => {
      app.ports.savedRecord.send(isHighScore);
    })
    .catch((err) => {
      console.error(err);
      // TODO: ネットワークエラー画面に飛ばす
    })
  // 全体のハイスコアを更新
  // const updatePublicHighScore = firebase.database().ref(`/music_infos/${csvFileName}`).transaction((musicInfo) => {
  //   if (musicInfo) {
  //     const bestCombo = record.combo > playRecord.bestCombo ? record.combo : playRecord.bestCombo;
  //     const bestScore = record.score > playRecord.bestScore ? record.score : playRecord.bestScore;
  //     const playCount = record.playCount + 1;
  //     return {
  //       bestCombo,
  //       bestScore,
  //       playCount
  //     }
  //   }
  // });
});



/* ---------------------------------
  過去の自分のプレイデータの取得
---------------------------------- */
app.ports.getOwnBestRecords.subscribe(uid => {
  firebase.database().ref(`/users/${uid}/play_records`).once('value').then(
    (snapshot) => {
      const datas = snapshot.val();
      if (datas) {
        const records = toArrFromObj(datas).map(record => {
          return {
            csvFileName: record.csv_file_name,
            bestCombo: record.best_combo,
            bestScore: record.best_score,
            playCount: record.play_count
          }
        });
        app.ports.gotOwnBestRecords.send(records);
      } else {
        app.ports.gotOwnBestRecords.send([]);
      }
    },
    (err) => {
      console.error(err);
      // TODO: ネットワークエラー画面に飛ばす
    }
  );
})

export const toArrFromObj = obj => {
  const arr = [];
  Object.keys(obj).forEach((key) => {
    arr.push(obj[key]);
  });
  return arr;
}