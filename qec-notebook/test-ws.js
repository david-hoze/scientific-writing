const WebSocket = require('ws');
const ws = new WebSocket('ws://localhost:8170');

ws.on('open', () => console.log('open'));
ws.on('message', (data) => {
  const msg = JSON.parse(data);
  const t = new Date().toISOString().substring(11, 23);
  console.log(t, msg.type || msg.status, JSON.stringify(msg).substring(0, 500));
});
ws.on('error', (e) => console.error('ERROR:', e.message));

setTimeout(() => {
  console.log('>> let quickConfig = SimConfig 500 defaultBPConfig 1');
  ws.send(JSON.stringify({type:'eval', cell_id:'d1', source:'let quickConfig = SimConfig 500 defaultBPConfig 1'}));
}, 1000);

setTimeout(() => {
  console.log('>> sweep ...');
  ws.send(JSON.stringify({type:'eval', cell_id:'e1', source:'sweep (sweepCodes "rep" repetitionCode [3,5]) (noiseRange 0.02 0.1 3) quickConfig'}));
}, 4000);

setTimeout(() => {
  console.log('done');
  ws.close();
  process.exit(0);
}, 30000);
