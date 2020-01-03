import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import {firebaseAuthSetUpSubscriber} from './js/firebaseAuth';
import {firebaseDBSetUpSubscriber} from './js/firebaseDB';
import {dataSetUpSubscriber} from './js/data';
import {audioSetUpSubscriber, BGM, SE} from './js/audio';
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
    firebase.database().ref(`/users/${user.uid}`).set(userInfo, detectedError)
      .then(() => {
        app.ports.onAuthStateChanged.send(userInfo);
      })
      .catch(detectedError)
  }
});



/* ---------------------------------
  Subscriber
---------------------------------- */
firebaseAuthSetUpSubscriber(app);
firebaseDBSetUpSubscriber(app);
dataSetUpSubscriber(app);
audioSetUpSubscriber(app, BGM(app), SE(app));
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