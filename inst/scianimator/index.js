(function($) {
	Index = {
		// Visible images
		images_vis: %s,

		/**
		 * Initialize the page
		 */
		init: function()
		{
			//$.fn.scianimator.defaults.debug = true;
			//$.fn.scianimator.defaults.theme = 'blue';

			// Construct 1st animator
			$('#scianimator1').scianimator({
				'images': Index.images_vis,
				'width': '640',
				'utf8': false,
				'theme': %s
			});

		}
	};

	$(document).ready(Index.init);
})(jQuery);
