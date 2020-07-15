import firebase from 'firebase/app';
import 'firebase/database';
import {detectedError, errorEvent} from '../index';
const uuidv4 = require('uuid/v4');



/* ---------------------------------
	Subscriber
---------------------------------- */
export const firebaseDBSetUpSubscriber = (app) => {
  const firebaseDBGet = (path) => firebase.database().ref(path).once('value');
  const firebaseDBSet = (path, data) => firebase.database().ref(path).set(data);

  /* ---------------------------------
	  GET
  ---------------------------------- */
  app.ports.getOwnRecord.subscribe(({uid, csvFileName}) => {
    firebaseDBGet(`/userDatas/${uid}/playRecords/${csvFileName}/`)
      .then((snapshot) => {
        const ownRecordDto = {csvFileName, uid, playRecord: snapshot.val()};
        app.ports.gotOwnRecord.send(ownRecordDto);
      })
      .catch(error => {
        console.error(error);
        detectedError(errorEvent.getOwnRecord, error.message, `<csvFileName: ${csvFileName}>`);
      });
  })

  app.ports.getPublicRecord.subscribe(csvFileName => {
    firebaseDBGet(`/publicRecords/${csvFileName}/`)
      .then((snapshot) => {
        const publicRecordDto = {csvFileName, bestRecords: snapshot.val()};
        app.ports.gotPublicRecord.send(publicRecordDto);
      })
      .catch(error => {
        console.error(error);
        detectedError(errorEvent.getPublicRecord, error.message, `<csvFileName: ${csvFileName}>`);
      });
  })

  app.ports.getUsers.subscribe(uidList => {
    const getUsers = uidList.map(uid => firebaseDBGet(`/users/${uid}/`));
    Promise.all(getUsers)
      .then(snapshots => {
        const users = snapshots.map(snapshot => snapshot.val());
        // 不正なuserは除く
        const userDtos = users.filter(user =>
          isValid(user) && isValid(user.uid) && isValid(user.name) && isValid(user.pictureUrl)
        );
        app.ports.gotUsers.send(userDtos);
      })
      .catch(error => {
        console.error(error);
        detectedError(errorEvent.getUsers, error.message, `<uidList: ${uidList}>`);
      });
  })

  app.ports.getUserSetting.subscribe(uid => {
    firebaseDBGet(`/userDatas/${uid}/settings/`)
      .then((snapshot) => {
        if (!snapshot.val()) {
          app.ports.gotUserSetting.send(null);
          return;
        }
        const userSettingDto = {
          currentMusicId: isValid(snapshot.val().currentMusicId) ? snapshot.val().currentMusicId : null,
          currentMode: isValid(snapshot.val().currentMode) ? snapshot.val().currentMode : null,
          notesSpeed: isValid(snapshot.val().notesSpeed) ? snapshot.val().notesSpeed : null,
          bgmVolume: isValid(snapshot.val().bgmVolume) ? snapshot.val().bgmVolume : null,
          seVolume: isValid(snapshot.val().seVolume) ? snapshot.val().seVolume : null
        }
        app.ports.gotUserSetting.send(userSettingDto);
      })
      .catch(error => {
        console.error(error);
        detectedError(errorEvent.getUserSetting, error.message, ``);
      });
  });

  app.ports.getMaintenanceMode.subscribe(() => {
    firebaseDBGet(`/maintenanceMode/`)
      .then((snapshot) => {
        app.ports.gotMaintenanceMode.send(snapshot.val());
      })
      .catch(error => {
        console.error(error);
        detectedError(errorEvent.getMaintenanceMode, error.message, ``);
      });
  })

  /* ---------------------------------
	  POST
  ---------------------------------- */
  app.ports.saveResult_.subscribe(result => {
    const resultId = uuidv4();
    firebaseDBSet(`/results/${result.csvFileName}/${result.uid}/${resultId}/`, result)
      .then(() => {
        app.ports.savedResult.send(null);
      })
      .catch(error => {
        const resultInfo = `{csvFileName: ${result.csvFileName}, combo: ${result.combo}, score: ${result.score}, createdAt: ${result.createdAt}}`;
        console.error(error);
        detectedError(errorEvent.saveResult, error.message, `<result: ${resultInfo}>`);
      });
  })

  app.ports.saveOwnRecord_.subscribe(({uid, csvFileName, playRecord}) => {
    firebaseDBSet(`/userDatas/${uid}/playRecords/${csvFileName}/`, playRecord)
      .then(() => {
        app.ports.savedOwnRecord.send(null);
      })
      .catch(error => {
        const playRecordInfo =
          !playRecord
            ? `null`
            : `{bestCombo: ${playRecord.bestCombo}, bestScore: ${playRecord.bestScore}, playCount: ${playRecord.playCount}}`;
        console.error(error);
        detectedError(
          errorEvent.saveOwnRecord,
          error.message,
          `<csvFileName: ${csvFileName}> <playRecord: ${playRecordInfo}>`
        );
      });
  })

  app.ports.savePublicRecord_.subscribe(({csvFileName, bestRecords}) => {
    firebaseDBSet(`/publicRecords/${csvFileName}/`, bestRecords)
      .then(() => {
        app.ports.savedPublicRecord.send(null);
      })
      .catch(error => {
        const bestRecordsInfo =
          !bestRecords || bestRecords === []
            ? `null`
            : bestRecords.map(bestRecord => `{uid: ${bestRecord.uid}, score: ${bestRecord.score}, createdAt: ${bestRecord.createdAt}}`).join();
        console.error(error);
        detectedError(
          errorEvent.savePublicRecord,
          error.message,
          `<csvFileName: ${csvFileName}> <bestRecords: ${bestRecordsInfo}>`
        );
      });
  })

  app.ports.saveCurrentMusicId.subscribe(({uid, currentMusicId}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/currentMusicId/`, currentMusicId)
      .then(() => {})
      .catch(error => {
        console.error(error);
        detectedError(errorEvent.saveCurrentMusicId, error.message, `<currentMusicId: ${currentMusicId}>`);
      });
  });

  app.ports.saveCurrentMode.subscribe(({uid, currentMode}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/currentMode/`, currentMode)
      .then(() => {})
      .catch(error => {
        console.error(error);
        detectedError(errorEvent.saveCurrentMode, error.message, `<currentMode: ${currentMode}>`);
      });
  });

  app.ports.saveNotesSpeed.subscribe(({uid, notesSpeed}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/notesSpeed/`, notesSpeed)
      .then(() => {})
      .catch(error => {
        console.error(error);
        detectedError(errorEvent.saveNotesSpeed, error.message, `<notesSpeed: ${notesSpeed}>`);
      });
  });

  app.ports.saveBgmVolume.subscribe(({uid, bgmVolume}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/bgmVolume/`, bgmVolume)
      .then(() => {})
      .catch(error => {
        console.error(error);
        detectedError(errorEvent.saveBgmVolume, error.message, `<bgmVolume: ${bgmVolume}>`);
      });
  });

  app.ports.saveSeVolume.subscribe(({uid, seVolume}) => {
    firebaseDBSet(`/userDatas/${uid}/settings/seVolume/`, seVolume)
      .then(() => {})
      .catch(error => {
        console.error(error);
        detectedError(errorEvent.saveSeVolume, error.message, `<seVolume: ${seVolume}>`);
      });
  });
}



/* ---------------------------------
	データチェック
---------------------------------- */
const isValid = (data) => {
  if (data !== false && data !== null && data !== undefined && data !== NaN && data !== '' ) {
    return true;
  } else {
    return false;
  }
}