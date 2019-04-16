export default {
  entry: '../dist/playground/main.js',
  dest: '../dist/playground/bundles/playground.umd.js',
  format: 'umd',
  moduleName: 'playground',
  globals: {
    'mojiito-core': 'mj.core',
    'mojiito-platform-browser': 'mj.platformBrowser'
  },
  context: 'this'
}
