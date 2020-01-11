import firebase from 'firebase/app';
import 'firebase/auth';
import 'firebase/database';
import 'firebase/storage';
import {detectedError, errorEvent} from '../index';
const uuidv4 = require('uuid/v4');



/* ---------------------------------
	Provider
---------------------------------- */
const googleAuthProvider = new firebase.auth.GoogleAuthProvider();
const twitterAuthProvider = new firebase.auth.TwitterAuthProvider();
const githubAuthProvider = new firebase.auth.GithubAuthProvider();



/* ---------------------------------
	Subscriber
---------------------------------- */
export function firebaseAuthSetUpSubscriber (app) {
  // サインイン
  app.ports.signInWithGoogle.subscribe(() => {
    firebase.auth().signInWithPopup(googleAuthProvider)
      .then(() => {})
      .catch(error => {
        detectedError(errorEvent.signIn, error, {provider: 'Google'});
        app.ports.canceledSignIn.send(null);
      });
  });

  app.ports.signInWithTwitter.subscribe(() => {
    firebase.auth().signInWithPopup(twitterAuthProvider)
      .then(() => {})
      .catch(error => {
        detectedError(errorEvent.signIn, error, {provider: 'Twitter'});
        app.ports.canceledSignIn.send(null);
      });
  });

  app.ports.signInWithGithub.subscribe(() => {
    firebase.auth().signInWithPopup(githubAuthProvider)
      .then(() => {})
      .catch(error => {
        detectedError(errorEvent.signIn, error, {provider: 'Github'});
        app.ports.canceledSignIn.send(null);
      });
  });

  // サインアウト
  app.ports.signOut.subscribe(() => {
    firebase.auth().signOut()
      .then(() => {})
      .catch(error => {
        const uid = firebase.auth().currentUser.uid;
        detectedError(errorEvent.signOut, error, {uid});
      });
  });

  // ユーザーネームの変更
  app.ports.saveUserName.subscribe(({uid, userName}) => {
    const currentUser = firebase.auth().currentUser;
    const updateAuth = currentUser.updateProfile({displayName: userName});
    const updateDB = firebase.database().ref(`/users/${uid}/userName/`).set(userName);
    Promise.all([updateAuth, updateDB])
      .then(() => {})
      .catch(error => {
        detectedError(errorEvent.saveUserName, error, {uid, userName});
      });
  });

  // ユーザーアイコンの変更
  app.ports.saveUserPicture.subscribe(({uid, event}) => {
    const file = event.target.files[0];
    if (!file) {
      detectedError(errorEvent.saveUserPicture, 'uploaded file is empty', {uid});
      return;
    }

    const fileType = getFileType(file.name);
    if (fileType === "") {
      app.ports.failedSaveUserPicture.send(null);
      return;
    }

    const filePath = `userIcons/${uid}/${file.name}`;
    const uploadFile = () => firebase.storage().ref().child(filePath).put(file);
    const getNewPictureUrl = () => firebase.storage().ref(filePath).getDownloadURL();

    uploadFile()
      .then(getNewPictureUrl)
      .then(url => {
        const currentUser = firebase.auth().currentUser;
        const updateAuth = currentUser.updateProfile({photoURL: url});
        const updateDB = firebase.database().ref(`/users/${uid}/pictureUrl/`).set(url);
        Promise.all([updateAuth, updateDB])
          .then(() => {
            app.ports.completedSaveUserPicture.send(url);
          })
          .catch(error => {
            detectedError(errorEvent.saveUserPicture, error, {uid, filePath});
          })
      })
      .catch(error => {
        detectedError(errorEvent.saveUserPicture, error, {uid, filePath});
      })
  });
}



/* ---------------------------------
	拡張子のチェック
---------------------------------- */
const getFileType = (fileName) => {
  const type = fileName.split('.');
  const typeStr = type[type.length - 1].toLowerCase();
  if (typeStr == 'png') {
    return "png";
  } else if (typeStr == 'jpg') {
    return "jpg";
  } else if (typeStr == 'jpeg') {
    return "jpeg";
  }
  return ""
}