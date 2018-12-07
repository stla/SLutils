(function($) {
	Index = {
		// Visible images
		images_vis: ["assets/images/Rplot01.png","assets/images/Rplot02.png","assets/images/Rplot03.png","assets/images/Rplot04.png","assets/images/Rplot05.png","assets/images/Rplot06.png","assets/images/Rplot07.png","assets/images/Rplot08.png","assets/images/Rplot09.png","assets/images/Rplot10.png"],

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
				'theme': 'dark'
			});

		}
	};

	$(document).ready(Index.init);
})(jQuery);
