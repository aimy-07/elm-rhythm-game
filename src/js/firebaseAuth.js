import firebase from 'firebase/app';
import 'firebase/auth';
import 'firebase/database';
import 'firebase/storage';
import {detectedError, errorEvent} from '../index';



/* ---------------------------------
	Provider
---------------------------------- */
const googleAuthProvider = new firebase.auth.GoogleAuthProvider();
const twitterAuthProvider = new firebase.auth.TwitterAuthProvider();
const githubAuthProvider = new firebase.auth.GithubAuthProvider();



/* ---------------------------------
	Subscriber
---------------------------------- */
export const firebaseAuthSetUpSubscriber = (app) => {
  // サインイン
  app.ports.signInWithGoogle.subscribe(() => {
    firebase.auth().signInWithPopup(googleAuthProvider)
      .then(() => {})
      .catch(error => {
        if (isCanceledSignIn(error)) {
          app.ports.canceledSignIn.send(null);
          return;
        }
        console.error(error);
        detectedError(errorEvent.signIn, error.message, `<provider: Google>`);
      });
  });

  app.ports.signInWithTwitter.subscribe(() => {
    firebase.auth().signInWithPopup(twitterAuthProvider)
      .then(() => {})
      .catch(error => {
        if (isCanceledSignIn(error)) {
          app.ports.canceledSignIn.send(null);
          return;
        }
        console.error(error);
        detectedError(errorEvent.signIn, error.message, `<provider: Twitter>`);
      });
  });

  app.ports.signInWithGithub.subscribe(() => {
    firebase.auth().signInWithPopup(githubAuthProvider)
      .then(() => {})
      .catch(error => {
        if (isCanceledSignIn(error)) {
          app.ports.canceledSignIn.send(null);
          return;
        }
        console.error(error);
        detectedError(errorEvent.signIn, error.message, `<provider: Github>`);
      });
  });

  // サインアウト
  app.ports.signOut.subscribe(() => {
    firebase.auth().signOut()
      .then(() => {})
      .catch(error => {
        console.error(error);
        detectedError(errorEvent.signOut, error.message, ``);
      });
  });

  // ユーザーネームの変更
  app.ports.saveUserName.subscribe(({uid, name}) => {
    const currentUser = firebase.auth().currentUser;
    const updateAuth = currentUser.updateProfile({displayName: name});
    const updateDB = firebase.database().ref(`/users/${uid}/name/`).set(name);
    Promise.all([updateAuth, updateDB])
      .then(() => {})
      .catch(error => {
        console.error(error);
        detectedError(errorEvent.saveUserName, error.message, `<name: ${name}>`);
      });
  });

  // ユーザーアイコンの変更
  app.ports.saveUserPicture.subscribe(({uid, event}) => {
    const file = event.target.files[0];
    if (!file) {
      detectedError(errorEvent.saveUserPicture, 'uploaded file is empty', ``);
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
            console.error(error);
            detectedError(errorEvent.saveUserPicture, error.message, `<fileName: ${file.name}>`);
          })
      })
      .catch(error => {
        console.error(error);
        detectedError(errorEvent.saveUserPicture, error.message, `<fileName: ${file.name}>`);
      })
  });
}



/* ---------------------------------
	サインインがキャンセルされたかどうかのチェック
---------------------------------- */
const isCanceledSignIn = error => {
  switch(error.code) {
    case 'auth/cancelled-popup-request':
    case 'auth/popup-closed-by-user':
      return true;
    default:
      return false;
  }
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