import firebase from 'firebase/app';
import 'firebase/auth';
import {detectedError} from '../index';



/* ---------------------------------
	Subscriber
---------------------------------- */
export function authSetUpSubscriber (app, googleAuthProvider) {
  // サインイン
  app.ports.signIn.subscribe(() => {
    firebase.auth().signInWithPopup(googleAuthProvider)
      .then(() => {
        // TODO: ここで ports.onAuthStateChanged を実行すれば、onAuthStateChangedは要らない？
      })
      .catch(detectedError);
  });

  // サインアウト
  app.ports.signOut.subscribe(() => {
    firebase.auth().signOut()
      .then(() => {})
      .catch(detectedError);
  });
}