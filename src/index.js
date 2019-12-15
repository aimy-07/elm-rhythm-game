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

app.ports.saveRecord.subscribe(({csvFileName, record}) => {
  const recordId = uuidv4();
  const createdAt = Date.now();
  firebase.database().ref(`/records/${csvFileName}/${recordId}/`).set(
    {
      uid: record.uid,
      combo: record.combo,
      score: record.score,
      createdAt: createdAt
    },
    (err) => {
      if (err) {
        console.error(err);
        // TODO: ネットワークエラー画面に飛ばす
      } else {
        app.ports.savedRecord.send(null);
      }
    }
  );
});



/* ---------------------------------
  過去の自分のプレイデータの取得
---------------------------------- */
app.ports.getOwnBestRecord.subscribe(({csvFileName, uid}) => {
  firebase.database().ref(`/records/${csvFileName}/`).orderByChild("uid").equalTo(uid).once('value').then(
    (snapshot) => {
      const datas = snapshot.val();
      if (datas) {
        const records = toArrFromObj(datas);
        const ownBestRecord = {
          uid: uid,
          combo: Math.max(...records.map(r => r.combo)),
          score: Math.max(...records.map(r => r.score)),
        }
        app.ports.gotOwnBestRecord.send(ownBestRecord);
      } else {
        const emptyRecord = {
          uid: uid,
          combo: 0,
          score: 0,
        }
        app.ports.gotOwnBestRecord.send(emptyRecord);
      }
    },
    (err) => {
      console.error(err);
      // TODO: ネットワークエラー画面に飛ばす
    }
  );
})

const toArrFromObj = obj => {
  const arr = [];
  Object.keys(obj).forEach((key) => {
    arr.push(obj[key]);
  });
  return arr;
}