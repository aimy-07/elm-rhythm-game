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

import {_} from 'underscore';
const uuidv4 = require('uuid/v4');


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
    const saveUser = firebase.database().ref(`/users/${user.uid}`).transaction(
      (userData) => {
        if (!userData) {
          return {
            userName: user.displayName,
            pictureUrl: user.photoURL,
          }
        } else {
          // 過去にログインしたことのあるユーザー
          return;
        }
      },
      (err) => {
        if (err) throw new Error(err);
      }
    );
    saveUser
      .then(() => {
        app.ports.onAuthStateChanged.send({
          uid: user.uid,
          userName: user.displayName,
          pictureUrl: user.photoURL,
        });
      })
      .catch((err) => {
        console.error(err);
        // TODO: ネットワークエラー画面に飛ばす
      })
  }
});



/* ---------------------------------
  サインイン/サインアウト
---------------------------------- */
app.ports.signIn.subscribe(() => {
  firebase.auth().signInWithPopup(googleAuthProvider)
    .then(() => {})
    .catch((err) => {
      console.error(err);
      // TODO: ネットワークエラー画面に飛ばす
    });
});

app.ports.signOut.subscribe(() => {
  firebase.auth().signOut()
    .then(() => {})
    .catch((err) => {
      console.error(err);
      // TODO: ネットワークエラー画面に飛ばす
    });
});



/* ---------------------------------
  Subscriber
---------------------------------- */
csvSetUpSubscriber(app);
audioSetUpSubscriber(app);
animationSetUpSubscriber(app);



/* ---------------------------------
  プレイリザルトの保存
---------------------------------- */
app.ports.saveRecord.subscribe(({uid, csvFileName, combo, score}) => {
  const recordId = uuidv4();
  const createdAt = Date.now();
  const saveRecord = firebase.database().ref(`/records/${csvFileName}/${uid}/${recordId}/`).set(
    {uid, csvFileName, combo, score, createdAt},
    (err) => {
      if (err) throw new Error(err);
    }
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
    (err) => {
      if (err) throw new Error(err);
    }
  );

  const updatePublicPlayRecord = firebase.database().ref(`/musicInfos/${csvFileName}/bestRecords`).transaction(
    (bestRecords) => {
      if (score == 0) {
        return bestRecords ? bestRecords : [];
      }
      if (bestRecords) {
        const newBestRecords = _.sortBy(bestRecords.concat({uid, bestScore: score}), (record => record.bestScore)).reverse();
        if (newBestRecords.length > 3) {
          return newBestRecords.slice(0, 3);
        } else {
          return newBestRecords
        }
      } else {
        return [{uid, bestScore: score}]
      }
    },
    (err) => {
      if (err) throw new Error(err);
    }
  );

  Promise.all([saveRecord, updateOwnPlayRecord, updatePublicPlayRecord])
    .then(() => {
      app.ports.savedRecord.send(isHighScore);
    })
    .catch((err) => {
      console.error(err);
      // TODO: ネットワークエラー画面に飛ばす
    })
});



/* ---------------------------------
  過去の自分のプレイデータの取得
---------------------------------- */
app.ports.getOwnBestRecords.subscribe(uid => {
  firebase.database().ref(`/users/${uid}/playRecords`).once('value').then(
    (snapshot) => {
      if (snapshot.val()) {
        const records = toArrFromObj(snapshot.val());
        app.ports.gotOwnBestRecords.send(records);
      } else {
        app.ports.gotOwnBestRecords.send([]);
      }
    },
    (err) => {
      console.error(err);
      // TODO: ネットワークエラー画面に飛ばす
    }
  )
});



// 連想配列を配列に変換する関数
export const toArrFromObj = obj => {
  const arr = [];
  Object.keys(obj).forEach((key) => {
    arr.push(obj[key]);
  });
  return arr;
}