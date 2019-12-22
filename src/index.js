import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import {authSetUpSubscriber} from './js/firebaseAuth';
import {databaseSetUpSubscriber} from './js/firebaseDB';
import {csvSetUpSubscriber} from './js/csv';
import {audioSetUpSubscriber} from './js/audio';
import {animationSetUpSubscriber} from './js/animation';

import firebase from 'firebase/app';
import 'firebase/auth';
import 'firebase/database';
import {firebaseConfig} from './config';



/* ---------------------------------
  Elm init
---------------------------------- */
const app = Elm.Main.init({
  node: document.getElementById('root')
});

registerServiceWorker();



/* ---------------------------------
  Firebase init
---------------------------------- */
firebase.initializeApp(firebaseConfig);

const googleAuthProvider = new firebase.auth.GoogleAuthProvider();



/* ---------------------------------
  onAuthStateChanged
---------------------------------- */
firebase.auth().onAuthStateChanged((user) => {
  if (!user) {
    // サインインしていない状態
    app.ports.onAuthStateChanged.send(null);
  } else {
    // サインイン済み
    const userInfo = {
      uid: user.uid,
      userName: user.displayName,
      pictureUrl: user.photoURL
    }
    const saveUser = firebase.database().ref(`/users/${user.uid}`).set(userInfo, detectedError);
    saveUser
      .then(() => {
        app.ports.onAuthStateChanged.send(userInfo);
      })
      .catch(detectedError)
  }
});



/* ---------------------------------
  Subscriber
---------------------------------- */
authSetUpSubscriber(app, googleAuthProvider);
databaseSetUpSubscriber(app);
csvSetUpSubscriber(app);
audioSetUpSubscriber(app);
animationSetUpSubscriber(app);



/* ---------------------------------
  エラー処理
---------------------------------- */
export const detectedError = (err) => {
  if (err) {
    console.error(err);
    app.ports.detectedError.send(null);
  }
}