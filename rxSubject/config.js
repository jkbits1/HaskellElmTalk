/**
 * Created by jk on 27/11/15.
 */

System.config({
  transpiler: 'typescript',
  typescriptOptions: {
    emitDecoratorMetadata: true
  },
  map: {
    app: "/rxjs/src",
    'rxjs5': 'https://cdnjs.cloudflare.com/ajax/libs/rxjs/5.0.0-beta.11/Rx.umd.js'
  },
  packages: {
    app: {
      main: 'main.ts',
      defaultExtension: 'ts'
    },
    'rxjs5': { defaultExtension: 'js' }
  }
});
