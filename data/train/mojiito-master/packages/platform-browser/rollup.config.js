export default {
  entry: '../../dist/packages-dist/platform-browser/mojiito/platform-browser.es5.js',
  dest: '../../dist/packages-dist/platform-browser/bundles/platform-browser.umd.js',
  format: 'umd',
  exports: 'named',
  moduleName: 'mj.platformBrowser',
  context: 'this',
  globals: {
    'mojiito-core': 'mj.core'
  }
};
