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
    'rx.all': 'https://cdnjs.cloudflare.com/ajax/libs/rxjs/3.1.1/rx.all.js'
  },
  packages: {
    app: {
      main: 'main.ts',
      defaultExtension: 'ts'
    }
  }
});
