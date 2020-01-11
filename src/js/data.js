import {detectedError, errorEvent} from '../index';



/* ---------------------------------
	Subscriber
---------------------------------- */
export function dataSetUpSubscriber (app) {
  // Jsonファイルの読み込み
  app.ports.loadMusicDataByJson.subscribe((musicId) => {
    const filePath = `./json/${musicId}.json`;

    getFile(
      filePath,
      (data => {
        app.ports.loadedMusicDataByJson.send(JSON.parse(data));
      }),
      (error => {
        const uid = firebase.auth().currentUser.uid;
        detectedError(errorEvent.loadMusicDataByJson, error, {filePath, uid});
      })
    )
  });

  // Csvファイルの読み込み
  app.ports.loadMusicDataByCsv.subscribe((csvFileName) => {
    const filePath = `./csv/${csvFileName}.csv`;

    getFile(
      filePath,
      (data => {
        const csvData = data.split("\n").map(csvRow =>
          csvRow.split(',').map(value => {
            const isValidValue = value && !isNaN(parseFloat(value));
            return isValidValue ? parseFloat(value) : null;
          })
        );
        app.ports.loadedMusicDataByCsv.send({csvFileName, csvData});
      }),
      (error => {
        const uid = firebase.auth().currentUser.uid;
        detectedError(errorEvent.loadMusicDataByCsv, error, {filePath, uid});
      })
    )
  });
}



/* ---------------------------------
	ファイル取得
---------------------------------- */
const getFile = (filePath, onload, onerror) => {
  const req = new XMLHttpRequest();
  req.open("get", filePath, true);
  req.send(null);
  req.onload = () => {
    if (req.status === 200) {
      onload(req.responseText);
      return;
    }
    onerror(`XMLHttpRequest error: [${req.status}]${req.statusText}`);
  };
  req.onerror = () => {
    onerror(`XMLHttpRequest error: [${req.status}]${req.statusText}`);
  };
}