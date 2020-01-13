import {detectedError, errorEvent} from '../index';



/* ---------------------------------
	Subscriber
---------------------------------- */
export function dataSetUpSubscriber (app) {
  // Jsonファイルの読み込み
  app.ports.loadMusicDataByJson.subscribe((musicId) => {
    getFile(
      `./json/${musicId}.json`,
      (data => {
        app.ports.loadedMusicDataByJson.send(JSON.parse(data));
      }),
      (errorMessage => {
        detectedError(errorEvent.loadMusicDataByJson, errorMessage, `<musicId: ${musicId}>`);
      })
    )
  });

  // Csvファイルの読み込み
  app.ports.loadMusicDataByCsv.subscribe((csvFileName) => {
    getFile(
      `./csv/${csvFileName}.csv`,
      (data => {
        const csvData = data.split("\n").map(csvRow =>
          csvRow.split(',').map(value => {
            const isValidValue = value && !isNaN(parseFloat(value));
            return isValidValue ? parseFloat(value) : null;
          })
        );
        app.ports.loadedMusicDataByCsv.send({csvFileName, csvData});
      }),
      (errorMessage => {
        detectedError(errorEvent.loadMusicDataByCsv, errorMessage, `<csvFileName: ${csvFileName}>`);
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