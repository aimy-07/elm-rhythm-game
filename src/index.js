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