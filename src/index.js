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
    // TODO: userDataの持ち方を変える
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
    .then(() => {
      // TODO: ここで ports.onAuthStateChanged を実行すれば、onAuthStateChangedは要らない？
    })
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
  UserSetting
---------------------------------- */
app.ports.getUserSetting.subscribe(uid => {
  firebase.database().ref(`/users/${uid}`).once('value').then(
    (snapshot) => {
      const data = snapshot.val();
      const currentMusicId = data.currentMusicId ? data.currentMusicId : null;
      const currentMode = data.currentMode ? data.currentMode : null;
      const notesSpeed = data.notesSpeed ? data.notesSpeed : null;
      app.ports.gotUserSetting.send({currentMusicId, currentMode, notesSpeed});
    },
    (err) => {
      if (err)  console.error(err);
      // TODO: ネットワークエラー画面に飛ばす
    }
  )
});

app.ports.saveCurrentMusicId.subscribe(({uid, currentMusicId}) => {
  firebase.database().ref(`/users/${uid}/currentMusicId/`).set(
    currentMusicId,
    (err) => {
      if (err)  console.error(err);
      // TODO: ネットワークエラー画面に飛ばす
    }
  );
});

app.ports.saveCurrentMode.subscribe(({uid, currentMode}) => {
  firebase.database().ref(`/users/${uid}/currentMode/`).set(
    currentMode,
    (err) => {
      if (err)  console.error(err);
      // TODO: ネットワークエラー画面に飛ばす
    }
  );
});

app.ports.saveNotesSpeed.subscribe(({uid, notesSpeed}) => {
  firebase.database().ref(`/users/${uid}/notesSpeed/`).set(
    notesSpeed,
    (err) => {
      if (err)  console.error(err);
      // TODO: ネットワークエラー画面に飛ばす
    }
  );
});



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
    (err) => {
      if (err) throw new Error(err);
    }
  );

  Promise.all([saveRecord, updateOwnPlayRecord, updatePublicPlayRecord])
    .then(() => {
      app.ports.savedRecord.send(isHighScore);
    })
    .catch((err) => {
      if (err)  console.error(err);
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
      if (err)  console.error(err);
      // TODO: ネットワークエラー画面に飛ばす
    }
  )
});



/* ---------------------------------
  ランキングデータの取得
---------------------------------- */
// TODO: firebaseルールでuserNameへのアクセスを許可
app.ports.getPublicRecords.subscribe(() => {
  firebase.database().ref(`/publicRecords`).once('value').then(
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
          .catch((err) => {
            if (err)  console.error(err);
            // TODO: ネットワークエラー画面に飛ばす
          })
      });
      Promise.all(getPublicRecords)
        .then((publicRecords) => {
          console.log('publicRecords', publicRecords);
          app.ports.gotPublicRecords.send(publicRecords);
        })
        .catch((err) => {
          if (err)  console.error(err);
          // TODO: ネットワークエラー画面に飛ばす
        })
    },
    (err) => {
      if (err)  console.error(err);
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